// blockr.io path input: a text field with server-side file and directory
// suggestions.
//
// Commit model (the design system's text fields): typing never commits,
// keystrokes only drive the suggestions. The Shiny input updates on a
// commit: Enter, blur, or picking a suggestion. While the typed text
// differs from the committed value the field shows the Enter button, and
// Escape reverts.
//
// The suggestions float on the menu surface of a field dropdown (the
// .blockr-select__dropdown classes) and are placed by Blockr.place, both
// from blockr.ui, which is loaded before this file.
(function() {
  "use strict";

  // Inline SVG icons (Bootstrap Icons)
  var FOLDER_ICON = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M.54 3.87.5 3a2 2 0 0 1 2-2h3.672a2 2 0 0 1 1.414.586l.828.828A2 2 0 0 0 9.828 3H13.5a2 2 0 0 1 2 2v1H.5v.5A1.5 1.5 0 0 1 2 5h12a1.5 1.5 0 0 1 1.5 1.5v6A1.5 1.5 0 0 1 14 14H2a1.5 1.5 0 0 1-1.5-1.5V5z"/></svg>';
  var FILE_ICON = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M4 0a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h8a2 2 0 0 0 2-2V5.5L9.5 0H4z"/><path d="M9.5 0v4a1 1 0 0 0 1 1H14L9.5 0z"/></svg>';

  function confirmIcon() {
    return (window.Blockr && Blockr.icons && Blockr.icons.confirm) || "";
  }

  // Per-input state
  var state = {};

  function getState(inputId) {
    if (!state[inputId]) {
      state[inputId] = {
        prefix: "",
        listUrl: null,
        activeIndex: -1,
        items: [],
        total: 0,
        debounceTimer: null,
        fetchSeq: 0,
        committed: "",
        everCommitted: false,
        placed: null
      };
    }
    return state[inputId];
  }

  // ----------------------------------------------------------------------
  // Replay queue: inside a dockview panel the input reaches the DOM only
  // when the panel mounts, which can be long after the server flushed its
  // restore messages. A message that finds no element is held here, keyed
  // by input id, and replayed when the element turns up.
  // ----------------------------------------------------------------------
  var pending = {};

  function replayPending(inputId) {
    var msgs = pending[inputId];
    if (!msgs) return;
    delete pending[inputId];
    if (msgs.setValue) applySetValue(msgs.setValue);
    if (msgs.status) applyStatus(msgs.status);
  }

  function enqueue(inputId, kind, msg) {
    if (!pending[inputId]) pending[inputId] = {};
    pending[inputId][kind] = msg;
  }

  // --------------------------------------------------------------------
  // Shiny input binding: reports on "change" only (Enter, blur, a pick).
  // --------------------------------------------------------------------
  if (window.Shiny && window.Shiny.InputBinding) {
    var pathBinding = new Shiny.InputBinding();
    $.extend(pathBinding, {
      find: function(scope) {
        return $(scope).find("input.io-path-text");
      },
      getValue: function(el) {
        return el.value;
      },
      setValue: function(el, value) {
        el.value = value;
      },
      subscribe: function(el, callback) {
        $(el).on("change.ioPathText", function() {
          callback(false);
        });
      },
      unsubscribe: function(el) {
        $(el).off(".ioPathText");
      },
      // Runs when Shiny binds the input; for dockview panels that is the
      // late bindAll on layout change, the first moment the element is
      // guaranteed to exist.
      initialize: function(el) {
        initPathInputs();
        replayPending(el.id);

        // A push the server made before this file was loaded reached a
        // Shiny with no handler for the message and was dropped. Say we
        // are here; the server answers with the value again.
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue(el.id + "_ready", Date.now(),
                              { priority: "event" });
        }
      }
    });
    Shiny.inputBindings.register(pathBinding, "blockr.io.pathText", 100);
    if (Shiny.inputBindings.setPriority) {
      Shiny.inputBindings.setPriority("blockr.io.pathText", 100);
    }
  }

  // --------------------------------------------------------------------
  // The Enter button (blockr.ui's .blockr-expr-confirm)
  // --------------------------------------------------------------------
  function ensureChip(inputId) {
    var input = document.getElementById(inputId);
    var field = input ? input.closest(".io-path-field") : null;
    if (!field) return null;
    var chip = field.querySelector(".blockr-expr-confirm");
    if (!chip) {
      chip = document.createElement("button");
      chip.type = "button";
      chip.className = "blockr-expr-confirm";
      chip.setAttribute("aria-label", "Apply (Enter)");
      chip.style.display = "none";
      var upload = field.querySelector(".io-path-upload");
      field.insertBefore(chip, upload);
      // Keeps focus in the input, so blur does not commit first.
      chip.addEventListener("mousedown", function(e) {
        e.preventDefault();
      });
      chip.addEventListener("click", function() {
        commit(inputId);
        closeDropdown(inputId);
      });
    }
    return chip;
  }

  function updateChip(inputId) {
    var chip = ensureChip(inputId);
    var input = document.getElementById(inputId);
    if (!chip || !input) return;
    var st = getState(inputId);
    if (input.value !== st.committed) {
      chip.style.display = "";
      chip.classList.remove("confirmed");
      chip.innerHTML = 'Enter <span class="blockr-kbd">↵</span>';
    } else if (st.everCommitted) {
      chip.style.display = "";
      chip.classList.add("confirmed");
      chip.innerHTML = confirmIcon();
    } else {
      chip.style.display = "none";
    }
    updateRequiredState(inputId);
  }

  // The required-empty cue (blockr.ui's .blockr-field--required-empty) on
  // a required field while it is empty; it clears as soon as there is text.
  function updateRequiredState(inputId) {
    var input = document.getElementById(inputId);
    if (!input) return;
    var container = input.closest(".io-path-input");
    var field = input.closest(".io-path-field");
    if (!container || !field) return;
    var required = container.getAttribute("data-required") === "true";
    var empty = !input.value || !input.value.trim();
    var on = required && empty;
    field.classList.toggle("blockr-field--required-empty", on);
    // The field wrapper around a labelled path input carries the label's *.
    var wrap = container.closest(".blockr-settings__field");
    if (wrap && required) wrap.classList.toggle("blockr-field--required-empty", on);
  }

  // Commit the current value: the only way it reaches Shiny (the binding
  // listens on "change").
  function commit(inputId) {
    var input = document.getElementById(inputId);
    if (!input) return;
    $(input).trigger("change");
  }

  function updatePrefixVisibility(inputId) {
    var st = getState(inputId);
    var input = document.getElementById(inputId);
    var prefixEl = document.getElementById(inputId + "_prefix");
    if (!input || !prefixEl) return;
    var isAbsolute = /^(\/|~|[A-Za-z]:)/.test(input.value);
    if (isAbsolute) {
      prefixEl.textContent = "";
      prefixEl.classList.remove("io-path-prefix--active");
    } else {
      prefixEl.textContent = st.prefix || "";
      prefixEl.classList.toggle("io-path-prefix--active", !!st.prefix);
    }
  }

  Shiny.addCustomMessageHandler("blockr-path-prefix", function(msg) {
    var st = getState(msg.id);
    st.prefix = msg.prefix || "";
    updatePrefixVisibility(msg.id);
  });

  Shiny.addCustomMessageHandler("blockr-path-list-url", function(msg) {
    var st = getState(msg.id);
    st.listUrl = msg.url;
  });

  // Set the value from the server. msg.silent: update the field without
  // reporting a change.
  function applySetValue(msg) {
    var el = document.getElementById(msg.id);
    var st = getState(msg.id);
    el.value = msg.value || "";
    st.committed = el.value;
    el.scrollLeft = el.scrollWidth;
    updatePrefixVisibility(msg.id);
    updateChip(msg.id);
    // An open list belongs to the old value's directory.
    st.items = [];
    closeDropdown(msg.id);
    if (!msg.silent) {
      $(el).trigger("change");
    }
  }

  Shiny.addCustomMessageHandler("blockr-path-set-value", function(msg) {
    if (document.getElementById(msg.id)) {
      applySetValue(msg);
    } else {
      enqueue(msg.id, "setValue", msg);
    }
  });

  // The data directory option's Set button: enabled while the field holds
  // a directory that differs from the saved one.
  var btnTimers = {};

  function buttonLabel(btn) {
    return btn.querySelector(".action-label") || btn;
  }

  function resetBtn(id) {
    var btn = document.getElementById(id);
    if (!btn) return;
    var label = buttonLabel(btn);
    if (btn.dataset.ioLabel) label.textContent = btn.dataset.ioLabel;
    btn.disabled = true;
    btnTimers[id] = null;
  }

  Shiny.addCustomMessageHandler("blockr-path-toggle-btn", function(msg) {
    var el = document.getElementById(msg.id);
    if (!el) return;
    // A confirmation on show resets on its own timer.
    if (btnTimers[msg.id]) return;
    el.disabled = !msg.enabled;
  });

  Shiny.addCustomMessageHandler("blockr-path-btn-success", function(msg) {
    var btn = document.getElementById(msg.id);
    if (!btn) return;
    if (btnTimers[msg.id]) clearTimeout(btnTimers[msg.id]);
    var label = buttonLabel(btn);
    if (!btn.dataset.ioLabel) btn.dataset.ioLabel = label.textContent;
    btn.disabled = true;
    label.textContent = "Done";
    btnTimers[msg.id] = setTimeout(function() {
      resetBtn(msg.id);
    }, 3000);
  });

  function formatSize(bytes) {
    if (bytes == null) return "";
    if (bytes < 1024) return bytes + " B";
    if (bytes < 1048576) return (bytes / 1024).toFixed(1) + " KB";
    return (bytes / 1048576).toFixed(1) + " MB";
  }

  // Fetch a directory listing from the registerDataObj endpoint. A
  // sequence number drops a slow response to an older query.
  function fetchListing(inputId, query) {
    var st = getState(inputId);
    if (!st.listUrl) return;

    var seq = ++st.fetchSeq;
    var url = st.listUrl + "&path=" + encodeURIComponent(query);
    fetch(url)
      .then(function(resp) { return resp.json(); })
      .then(function(data) {
        if (seq !== st.fetchSeq) return;
        st.items = data.items || [];
        st.total = data.total || st.items.length;
        st.queryBase = data.base || "";
        st.activeIndex = -1;
        renderDropdown(inputId);
      })
      .catch(function() {
        if (seq !== st.fetchSeq) return;
        st.items = [];
        st.total = 0;
        renderDropdown(inputId);
      });
  }

  function isOpen(inputId) {
    var dropdown = document.getElementById(inputId + "_dropdown");
    return !!dropdown && dropdown.style.display === "block";
  }

  function renderDropdown(inputId) {
    var st = getState(inputId);
    var dropdown = document.getElementById(inputId + "_dropdown");
    var input = document.getElementById(inputId);
    var field = input ? input.closest(".io-path-field") : null;
    if (!dropdown || !field) return;

    if (st.items.length === 0) {
      closeDropdown(inputId);
      return;
    }

    var html = "";
    for (var i = 0; i < st.items.length; i++) {
      var item = st.items[i];
      var icon = item.isdir ? FOLDER_ICON : FILE_ICON;
      var size = item.isdir ? "" :
        '<span class="blockr-select__opt-label">' + formatSize(item.size) + "</span>";
      html += '<div class="blockr-select__option io-path-option' +
        (i === st.activeIndex ? " blockr-select__option--highlighted" : "") +
        '" role="option" data-index="' + i + '">' +
        '<span class="io-path-option__icon">' + icon + "</span>" +
        '<span class="io-path-option__name">' + escapeHtml(item.name) + "</span>" +
        size + "</div>";
    }
    if (st.total > st.items.length) {
      html += '<div class="blockr-select__empty">' + st.items.length +
        " of " + st.total + " shown. Type to narrow the list.</div>";
    }

    dropdown.innerHTML = html;

    // On <body>, out of reach of the clipping of dock panels.
    if (dropdown.parentElement !== document.body) {
      document.body.appendChild(dropdown);
    }
    if (!dropdown.dataset.ioWired) {
      dropdown.dataset.ioWired = "true";
      // preventDefault keeps the input focused.
      dropdown.addEventListener("mousedown", function(e) {
        e.preventDefault();
        var itemEl = e.target.closest(".io-path-option");
        if (itemEl) {
          selectItem(inputId, parseInt(itemEl.dataset.index, 10));
        }
      });
    }
    dropdown.style.display = "block";
    if (!st.placed) {
      st.placed = Blockr.place(dropdown, field, { minWidth: 190 });
    } else {
      st.placed.update();
    }

    if (st.activeIndex >= 0) {
      var active = dropdown.querySelector(".blockr-select__option--highlighted");
      if (active && active.scrollIntoView) {
        active.scrollIntoView({ block: "nearest" });
      }
    }
  }

  function escapeHtml(str) {
    var div = document.createElement("div");
    div.textContent = str;
    return div.innerHTML;
  }

  // The directory part of the current value.
  function getBase(value) {
    var lastSlash = value.lastIndexOf("/");
    if (lastSlash >= 0) {
      return value.substring(0, lastSlash + 1);
    }
    if (/^(~|[A-Za-z]:)$/.test(value)) {
      return value + "/";
    }
    return "";
  }

  // A pick is an explicit choice, so it commits.
  function selectItem(inputId, idx) {
    var st = getState(inputId);
    var item = st.items[idx];
    if (!item) return;

    var input = document.getElementById(inputId);
    if (!input) return;

    var base = st.queryBase || getBase(input.value);

    if (item.isdir) {
      // A folder: go into it and list it.
      input.value = base + item.name + "/";
      commit(inputId);
      clearTimeout(st.debounceTimer);
      st.debounceTimer = setTimeout(function() {
        fetchListing(inputId, input.value);
      }, 100);
    } else {
      input.value = base + item.name;
      commit(inputId);
      closeDropdown(inputId);
    }
  }

  function closeDropdown(inputId) {
    var st = getState(inputId);
    var dropdown = document.getElementById(inputId + "_dropdown");
    if (st.placed) {
      st.placed.stop();
      st.placed = null;
    }
    if (dropdown) {
      dropdown.style.display = "none";
    }
    st.activeIndex = -1;
  }

  // Drop state and body-mounted lists of inputs that left the page.
  function cleanupOrphans() {
    Object.keys(state).forEach(function(id) {
      if (document.getElementById(id)) return;
      closeDropdown(id);
      var dropdown = document.getElementById(id + "_dropdown");
      if (dropdown && dropdown.parentElement === document.body) {
        dropdown.parentElement.removeChild(dropdown);
      }
      delete state[id];
    });
  }

  function realFileInput(uploadTarget) {
    var fileEl = document.getElementById(uploadTarget);
    var wrapper = fileEl ? fileEl.closest(".shiny-input-container") : null;
    return wrapper ? wrapper.querySelector('input[type="file"]') : null;
  }

  function initPathInputs() {
    cleanupOrphans();

    var inputs = document.querySelectorAll(".io-path-text");
    inputs.forEach(function(input) {
      if (input.dataset.ioInit) return;
      input.dataset.ioInit = "true";

      var inputId = input.id;
      replayPending(inputId);
      var st = getState(inputId);
      st.committed = input.value;
      ensureChip(inputId);
      updateRequiredState(inputId);
      updatePrefixVisibility(inputId);

      // Every commit (Enter, blur, a pick, a trigger from here).
      $(input).on("change.ioPathChip", function() {
        st.committed = input.value;
        st.everCommitted = true;
        updateChip(inputId);
      });

      // Typing feeds the list and arms the Enter button, nothing more.
      input.addEventListener("input", function() {
        updatePrefixVisibility(inputId);
        updateChip(inputId);
        clearTimeout(st.debounceTimer);
        st.debounceTimer = setTimeout(function() {
          fetchListing(inputId, input.value);
        }, 200);
      });

      input.addEventListener("focus", function() {
        input.scrollLeft = 0;
        if (st.listUrl) {
          fetchListing(inputId, input.value);
        }
      });

      // The native change event, fired before blur when the value was
      // edited, has committed already.
      input.addEventListener("blur", function() {
        input.scrollLeft = input.scrollWidth;
        setTimeout(function() {
          closeDropdown(inputId);
        }, 200);
      });

      // Upload: the icon and a drop on the field both feed the hidden
      // fileInput.
      var container = input.closest(".io-path-input");
      var uploadTarget = container && container.getAttribute("data-upload-target");

      if (uploadTarget && container) {
        var uploadBtn = container.querySelector(".io-path-upload");
        if (uploadBtn) {
          uploadBtn.addEventListener("click", function(e) {
            e.preventDefault();
            var realInput = realFileInput(uploadTarget);
            if (realInput) realInput.click();
          });
        }

        var field = container.querySelector(".io-path-field");

        field.addEventListener("dragover", function(e) {
          e.preventDefault();
          e.stopPropagation();
          field.classList.add("io-path-field--dragover");
        });

        field.addEventListener("dragleave", function(e) {
          e.preventDefault();
          field.classList.remove("io-path-field--dragover");
        });

        field.addEventListener("drop", function(e) {
          e.preventDefault();
          e.stopPropagation();
          field.classList.remove("io-path-field--dragover");

          var files = e.dataTransfer.files;
          if (!files.length) return;

          var realInput = realFileInput(uploadTarget);
          if (realInput) {
            var dt = new DataTransfer();
            for (var i = 0; i < files.length; i++) dt.items.add(files[i]);
            realInput.files = dt.files;
            $(realInput).trigger("change");
          }
        });
      }

      // Arrows move through the list; Enter picks the keyboard row or
      // commits the typed value; Escape closes the list, or else reverts
      // an edit. Either way the key stops here, so a gear tray the field
      // sits in stays open.
      input.addEventListener("keydown", function(e) {
        var open = isOpen(inputId);

        if (e.key === "ArrowDown" && open) {
          e.preventDefault();
          st.activeIndex = Math.min(st.activeIndex + 1, st.items.length - 1);
          renderDropdown(inputId);
        } else if (e.key === "ArrowUp" && open) {
          e.preventDefault();
          st.activeIndex = Math.max(st.activeIndex - 1, 0);
          renderDropdown(inputId);
        } else if (e.key === "Enter") {
          e.preventDefault();
          if (open && st.activeIndex >= 0) {
            selectItem(inputId, st.activeIndex);
          } else {
            commit(inputId);
            closeDropdown(inputId);
          }
        } else if (e.key === "Escape") {
          if (open) {
            e.stopPropagation();
            closeDropdown(inputId);
          } else if (input.value !== st.committed) {
            e.stopPropagation();
            input.value = st.committed;
            updatePrefixVisibility(inputId);
            updateChip(inputId);
          }
        }
      });
    });
  }

  // The status badge under the field: neutral, or danger for an error.
  function applyStatus(msg) {
    var el = document.getElementById(msg.id + "_status");
    if (!el) return;
    if (msg.text && msg.state && msg.state !== "none") {
      var danger = msg.state === "error";
      el.innerHTML = '<span class="blockr-badge io-path-badge' +
        (danger ? " blockr-badge--danger io-path-badge--danger" : "") + '">' +
        escapeHtml(msg.text) + "</span>";
    } else {
      el.innerHTML = "";
    }
  }

  Shiny.addCustomMessageHandler("blockr-path-status", function(msg) {
    if (document.getElementById(msg.id + "_status")) {
      applyStatus(msg);
    } else {
      enqueue(msg.id, "status", msg);
    }
  });

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initPathInputs);
  } else {
    initPathInputs();
  }

  // New content from a render.
  $(document).on("shiny:value", function() {
    setTimeout(initPathInputs, 100);
  });
})();
