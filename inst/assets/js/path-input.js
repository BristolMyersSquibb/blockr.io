// blockr path-input widget
// Custom message handlers and autocomplete for server-side file browsing.
//
// Commit model (blockr design system, decided 2026-07-02): typing never
// commits — keystrokes only drive the autocomplete dropdown. The Shiny input
// updates on COMMIT only: Enter, blur, or a dropdown selection. While the
// typed text differs from the committed value, an "Enter ↵" chip is shown;
// committing swaps it for a faded check mark. See
// blockr.docs/design-system/target/design-system.html §5.5.
(function() {
  "use strict";

  // Inline SVG icons (Bootstrap Icons)
  var FOLDER_ICON = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" viewBox="0 0 16 16"><path d="M.54 3.87.5 3a2 2 0 0 1 2-2h3.672a2 2 0 0 1 1.414.586l.828.828A2 2 0 0 0 9.828 3H13.5a2 2 0 0 1 2 2v1H.5v.5A1.5 1.5 0 0 1 2 5h12a1.5 1.5 0 0 1 1.5 1.5v6A1.5 1.5 0 0 1 14 14H2a1.5 1.5 0 0 1-1.5-1.5V5z"/></svg>';
  var FILE_ICON = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" viewBox="0 0 16 16"><path d="M4 0a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h8a2 2 0 0 0 2-2V5.5L9.5 0H4z"/><path d="M9.5 0v4a1 1 0 0 0 1 1H14L9.5 0z"/></svg>';
  var CHECK_ICON = '<svg xmlns="http://www.w3.org/2000/svg" width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="20 6 9 17 4 12"/></svg>';

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
        everCommitted: false
      };
    }
    return state[inputId];
  }

  // --------------------------------------------------------------------
  // Shiny input binding: commit on "change" only (Enter / blur / dropdown
  // selection). This replaces the default text binding, which would send
  // every keystroke after a debounce — the source of half-typed paths
  // reaching the pipeline.
  // --------------------------------------------------------------------
  if (window.Shiny && window.Shiny.InputBinding) {
    var pathBinding = new Shiny.InputBinding();
    $.extend(pathBinding, {
      find: function(scope) {
        return $(scope).find("input.blockr-path-text");
      },
      getValue: function(el) {
        return el.value;
      },
      setValue: function(el, value) {
        el.value = value;
      },
      subscribe: function(el, callback) {
        $(el).on("change.blockrPathText", function() {
          callback(false);
        });
      },
      unsubscribe: function(el) {
        $(el).off(".blockrPathText");
      }
    });
    Shiny.inputBindings.register(pathBinding, "blockr.pathText", 100);
    if (Shiny.inputBindings.setPriority) {
      Shiny.inputBindings.setPriority("blockr.pathText", 100);
    }
  }

  // --------------------------------------------------------------------
  // Commit chip ("Enter ↵" while dirty → faded ✓ once applied)
  // --------------------------------------------------------------------
  function ensureChip(inputId) {
    var input = document.getElementById(inputId);
    var field = input ? input.closest(".blockr-path-input-field") : null;
    if (!field) return null;
    var chip = field.querySelector(".blockr-path-commit");
    if (!chip) {
      chip = document.createElement("button");
      chip.type = "button";
      chip.className = "blockr-path-commit";
      chip.title = "Apply (Enter)";
      chip.setAttribute("aria-label", "Apply (Enter)");
      chip.style.display = "none";
      var upload = field.querySelector(".blockr-path-upload-btn");
      field.insertBefore(chip, upload);
      // preventDefault keeps focus in the input so blur doesn't fire first
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
      // dirty: armed chip — verb + muted glyph (design system §5.5)
      chip.style.display = "";
      chip.classList.remove("confirmed");
      chip.innerHTML = 'Enter <span class="blockr-kbd">↵</span>';
    } else if (st.everCommitted) {
      // committed: faded check — nothing left to do
      chip.style.display = "";
      chip.classList.add("confirmed");
      chip.innerHTML = CHECK_ICON;
    } else {
      chip.style.display = "none";
    }
    updateRequiredState(inputId);
  }

  // Soft amber "needs a value" cue on required-but-empty fields, mirroring
  // blockr.viz's .dd-role-required-empty. Toggled off the visible value, so
  // it clears the moment the user types (and never shows on optional fields).
  function updateRequiredState(inputId) {
    var input = document.getElementById(inputId);
    if (!input) return;
    var container = input.closest(".blockr-path-input");
    var field = input.closest(".blockr-path-input-field");
    if (!container || !field) return;
    var required = container.getAttribute("data-required") === "true";
    var empty = !input.value || !input.value.trim();
    field.classList.toggle("blockr-path-required-empty", required && empty);
  }

  // Commit the current input value: this is the ONLY path through which
  // the value reaches Shiny (the binding listens on "change").
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
      prefixEl.classList.remove("blockr-path-prefix-active");
    } else {
      prefixEl.textContent = st.prefix || "";
      prefixEl.classList.toggle("blockr-path-prefix-active", !!st.prefix);
    }
  }

  // Custom message handler: update prefix text
  Shiny.addCustomMessageHandler("blockr-path-prefix", function(msg) {
    var st = getState(msg.id);
    st.prefix = msg.prefix || "";
    updatePrefixVisibility(msg.id);
  });

  // Custom message handler: store list_dir endpoint URL
  Shiny.addCustomMessageHandler("blockr-path-list-url", function(msg) {
    var st = getState(msg.id);
    st.listUrl = msg.url;
  });

  // Custom message handler: set input value programmatically
  // msg.silent: if true, only update the display without triggering change
  Shiny.addCustomMessageHandler("blockr-path-set-value", function(msg) {
    var el = document.getElementById(msg.id);
    if (el) {
      var st = getState(msg.id);
      el.value = msg.value || "";
      // Programmatic values are already applied — treat as committed
      st.committed = el.value;
      // Scroll to end so filename is visible
      el.scrollLeft = el.scrollWidth;
      updatePrefixVisibility(msg.id);
      updateChip(msg.id);
      if (!msg.silent) {
        $(el).trigger("change");
      }
    }
  });

  // Button state for success animation timers
  var btnTimers = {};

  function resetBtn(id) {
    var btn = document.getElementById(id);
    if (!btn) return;
    btn.classList.remove("blockr-datadir-btn-success");
    btn.innerHTML = '<span class="action-label">Set data directory</span>';
    btn.disabled = true;
    btnTimers[id] = null;
  }

  // Custom message handler: enable/disable a button (clears success state)
  Shiny.addCustomMessageHandler("blockr-path-toggle-btn", function(msg) {
    var el = document.getElementById(msg.id);
    if (!el) return;
    // Don't interrupt success animation — let the timer handle the reset
    if (btnTimers[msg.id]) return;
    el.disabled = !msg.enabled;
    el.classList.remove("blockr-datadir-btn-success");
  });

  // Custom message handler: button success animation
  Shiny.addCustomMessageHandler("blockr-path-btn-success", function(msg) {
    var btn = document.getElementById(msg.id);
    if (!btn) return;
    // Clear any pending timer
    if (btnTimers[msg.id]) clearTimeout(btnTimers[msg.id]);
    // Switch to success state
    btn.disabled = true;
    btn.classList.add("blockr-datadir-btn-success");
    btn.innerHTML = '<span class="blockr-path-check"><svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="20 6 9 17 4 12"/></svg></span> Set successfully';
    // Reset after 3 seconds
    btnTimers[msg.id] = setTimeout(function() {
      resetBtn(msg.id);
    }, 3000);
  });

  // Format file size
  function formatSize(bytes) {
    if (bytes == null) return "";
    if (bytes < 1024) return bytes + " B";
    if (bytes < 1048576) return (bytes / 1024).toFixed(1) + " KB";
    return (bytes / 1048576).toFixed(1) + " MB";
  }

  // Fetch directory listing from the registerDataObj endpoint.
  // A per-input sequence number discards stale responses: without it a slow
  // response for an older query can overwrite the dropdown for a newer one.
  function fetchListing(inputId, query) {
    var st = getState(inputId);
    if (!st.listUrl) return;

    var seq = ++st.fetchSeq;
    var url = st.listUrl + "&path=" + encodeURIComponent(query);
    fetch(url)
      .then(function(resp) { return resp.json(); })
      .then(function(data) {
        if (seq !== st.fetchSeq) return; // stale response — ignore
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

  // Position the (body-mounted) dropdown under its input field
  function positionDropdown(inputId) {
    var dropdown = document.getElementById(inputId + "_dropdown");
    var input = document.getElementById(inputId);
    var field = input ? input.closest(".blockr-path-input-field") : null;
    if (!dropdown || !field) return;
    var rect = field.getBoundingClientRect();
    dropdown.style.position = "fixed";
    dropdown.style.top = rect.bottom + "px";
    dropdown.style.left = rect.left + "px";
    dropdown.style.right = "auto";
    dropdown.style.width = rect.width + "px";
  }

  // Keep open dropdowns glued to their inputs on scroll/resize (the
  // dropdown lives on document.body with fixed coords, so any scrolling
  // ancestor would otherwise leave it floating detached).
  var repositionScheduled = false;
  function repositionOpenDropdowns() {
    if (repositionScheduled) return;
    repositionScheduled = true;
    requestAnimationFrame(function() {
      repositionScheduled = false;
      Object.keys(state).forEach(function(id) {
        var dropdown = document.getElementById(id + "_dropdown");
        if (!dropdown || dropdown.style.display !== "block") return;
        if (!document.getElementById(id)) {
          dropdown.style.display = "none";
          return;
        }
        positionDropdown(id);
      });
    });
  }
  window.addEventListener("scroll", repositionOpenDropdowns, true);
  window.addEventListener("resize", repositionOpenDropdowns);

  // Render dropdown
  function renderDropdown(inputId) {
    var st = getState(inputId);
    var dropdown = document.getElementById(inputId + "_dropdown");
    if (!dropdown) return;

    if (st.items.length === 0) {
      dropdown.style.display = "none";
      st.activeIndex = -1;
      return;
    }

    var html = "";
    for (var i = 0; i < st.items.length; i++) {
      var item = st.items[i];
      var icon = item.isdir ? FOLDER_ICON : FILE_ICON;
      var sizeText = item.isdir ? "" : '<span class="blockr-path-size">' + formatSize(item.size) + '</span>';
      html += '<div class="blockr-path-dropdown-item' + (i === st.activeIndex ? ' active' : '') + '" data-index="' + i + '">' +
        '<span class="blockr-path-icon">' + icon + '</span>' +
        '<span class="blockr-path-name">' + escapeHtml(item.name) + '</span>' +
        sizeText +
        '</div>';
    }
    if (st.total > st.items.length) {
      html += '<div class="blockr-path-dropdown-footer">Showing ' +
        st.items.length + " of " + st.total + " matches</div>";
    }

    dropdown.innerHTML = html;

    // Move dropdown to document.body so it escapes any overflow:auto/hidden
    // ancestors (e.g. dockview panels). Position with fixed coords relative
    // to the input field.
    if (dropdown.parentElement !== document.body) {
      document.body.appendChild(dropdown);
    }
    if (!dropdown.dataset.blockrWired) {
      dropdown.dataset.blockrWired = "true";
      // One delegated handler instead of per-render listeners.
      // preventDefault keeps the input focused (no blur-close race).
      dropdown.addEventListener("mousedown", function(e) {
        e.preventDefault();
        var itemEl = e.target.closest(".blockr-path-dropdown-item");
        if (itemEl) {
          selectItem(inputId, parseInt(itemEl.dataset.index, 10));
        }
      });
    }
    positionDropdown(inputId);
    dropdown.style.display = "block";

    // Keep the active item in view during keyboard navigation
    if (st.activeIndex >= 0) {
      var active = dropdown.querySelector(".blockr-path-dropdown-item.active");
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

  // Compute the directory base from the current input value.
  // If the value contains "/", base is everything up to and including the last
  // "/".  Otherwise, if the value is non-empty the server treated the whole
  // value as a directory name (e.g. "~"), so the base is value + "/".
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

  // Select an item from the dropdown. A selection is an explicit choice,
  // so it always commits.
  function selectItem(inputId, idx) {
    var st = getState(inputId);
    var item = st.items[idx];
    if (!item) return;

    var input = document.getElementById(inputId);
    if (!input) return;

    var base = st.queryBase || getBase(input.value);

    if (item.isdir) {
      // Folder: set base + name + "/", commit, and re-query to descend
      input.value = base + item.name + "/";
      commit(inputId);
      clearTimeout(st.debounceTimer);
      st.debounceTimer = setTimeout(function() {
        fetchListing(inputId, input.value);
      }, 100);
    } else {
      // File: set base + name, commit, and close dropdown
      input.value = base + item.name;
      commit(inputId);
      closeDropdown(inputId);
    }
  }

  function closeDropdown(inputId) {
    var dropdown = document.getElementById(inputId + "_dropdown");
    if (dropdown) {
      dropdown.style.display = "none";
      // Reset fixed positioning so CSS defaults apply on next open
      dropdown.style.position = "";
      dropdown.style.top = "";
      dropdown.style.left = "";
      dropdown.style.width = "";
    }
    var st = getState(inputId);
    st.activeIndex = -1;
  }

  // Remove state and body-mounted dropdowns for inputs no longer in the DOM
  // (e.g. a deleted block) so they don't accumulate.
  function cleanupOrphans() {
    Object.keys(state).forEach(function(id) {
      if (document.getElementById(id)) return;
      var dropdown = document.getElementById(id + "_dropdown");
      if (dropdown && dropdown.parentElement === document.body) {
        dropdown.parentElement.removeChild(dropdown);
      }
      delete state[id];
    });
  }

  // Initialize path inputs on document ready and after Shiny re-renders
  function initPathInputs() {
    cleanupOrphans();

    var inputs = document.querySelectorAll(".blockr-path-text");
    inputs.forEach(function(input) {
      if (input.dataset.blockrInit) return;
      input.dataset.blockrInit = "true";

      var inputId = input.id;
      var st = getState(inputId);
      st.committed = input.value;
      ensureChip(inputId);
      updateRequiredState(inputId);

      // Track every commit (Enter, blur, selection, programmatic trigger):
      // jQuery-bound so both native change events and $().trigger("change")
      // land here.
      $(input).on("change.blockrPathChip", function() {
        st.committed = input.value;
        st.everCommitted = true;
        updateChip(inputId);
      });

      // Typing: only feeds the dropdown (debounced) and arms the chip.
      // Nothing reaches Shiny from here.
      input.addEventListener("input", function() {
        updatePrefixVisibility(inputId);
        updateChip(inputId);
        clearTimeout(st.debounceTimer);
        st.debounceTimer = setTimeout(function() {
          fetchListing(inputId, input.value);
        }, 200);
      });

      // Focus: trigger listing if URL available
      input.addEventListener("focus", function() {
        input.scrollLeft = 0;
        if (st.listUrl) {
          fetchListing(inputId, input.value);
        }
      });

      // Blur: the native change event (fired before blur when the value
      // was edited) has already committed. Scroll to end so the filename
      // is visible, then close the dropdown.
      input.addEventListener("blur", function() {
        input.scrollLeft = input.scrollWidth;
        setTimeout(function() {
          closeDropdown(inputId);
        }, 200);
      });

      // Upload integration: wire icon click + drag-and-drop to hidden fileInput
      var container = input.closest(".blockr-path-input");
      var uploadTarget = container && container.getAttribute("data-upload-target");

      if (uploadTarget && container) {
        var uploadBtn = container.querySelector(".blockr-path-upload-btn");
        if (uploadBtn) {
          uploadBtn.addEventListener("click", function(e) {
            e.preventDefault();
            var fileEl = document.getElementById(uploadTarget);
            var wrapper = fileEl ? fileEl.closest(".shiny-input-container") : null;
            var realInput = wrapper ? wrapper.querySelector('input[type="file"]') : null;
            if (realInput) realInput.click();
          });
        }

        var field = container.querySelector(".blockr-path-input-field");

        field.addEventListener("dragover", function(e) {
          e.preventDefault();
          e.stopPropagation();
          field.classList.add("blockr-path-dragover");
        });

        field.addEventListener("dragleave", function(e) {
          e.preventDefault();
          field.classList.remove("blockr-path-dragover");
        });

        field.addEventListener("drop", function(e) {
          e.preventDefault();
          e.stopPropagation();
          field.classList.remove("blockr-path-dragover");

          var files = e.dataTransfer.files;
          if (!files.length) return;

          var fileEl = document.getElementById(uploadTarget);
          var wrapper = fileEl ? fileEl.closest(".shiny-input-container") : null;
          var realInput = wrapper ? wrapper.querySelector('input[type="file"]') : null;
          if (realInput) {
            var dt = new DataTransfer();
            for (var i = 0; i < files.length; i++) dt.items.add(files[i]);
            realInput.files = dt.files;
            $(realInput).trigger("change");
          }
        });
      }

      // Keyboard: arrows navigate the dropdown; Enter selects the active
      // item, or commits the typed value when nothing is highlighted;
      // Escape closes the dropdown without committing.
      input.addEventListener("keydown", function(e) {
        var dropdown = document.getElementById(inputId + "_dropdown");
        var open = dropdown && dropdown.style.display === "block";

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
          closeDropdown(inputId);
        }
      });
    });
  }

  // Custom message handler: file-type status badge
  Shiny.addCustomMessageHandler("blockr-path-status", function(msg) {
    var el = document.getElementById(msg.id + "_status");
    if (!el) return;
    if (msg.state === "success" && msg.text) {
      el.innerHTML = '<span class="blockr-path-badge blockr-path-badge-success">' +
        escapeHtml(msg.text) + '</span>';
    } else if (msg.state === "error" && msg.text) {
      el.innerHTML = '<span class="blockr-path-badge blockr-path-badge-error">' +
        escapeHtml(msg.text) + '</span>';
    } else if (msg.state === "info" && msg.text) {
      el.innerHTML = '<span class="blockr-path-badge blockr-path-badge-info">' +
        escapeHtml(msg.text) + '</span>';
    } else {
      el.innerHTML = "";
    }
  });

  // Run on DOM ready
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initPathInputs);
  } else {
    initPathInputs();
  }

  // Re-init after Shiny renders new content
  $(document).on("shiny:value", function() {
    setTimeout(initPathInputs, 100);
  });
})();
