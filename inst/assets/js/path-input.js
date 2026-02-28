// blockr path-input widget
// Custom message handlers and autocomplete for server-side file browsing
(function() {
  "use strict";

  // Inline SVG icons (Bootstrap Icons)
  var FOLDER_ICON = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" viewBox="0 0 16 16"><path d="M.54 3.87.5 3a2 2 0 0 1 2-2h3.672a2 2 0 0 1 1.414.586l.828.828A2 2 0 0 0 9.828 3H13.5a2 2 0 0 1 2 2v1H.5v.5A1.5 1.5 0 0 1 2 5h12a1.5 1.5 0 0 1 1.5 1.5v6A1.5 1.5 0 0 1 14 14H2a1.5 1.5 0 0 1-1.5-1.5V5z"/></svg>';
  var FILE_ICON = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" viewBox="0 0 16 16"><path d="M4 0a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h8a2 2 0 0 0 2-2V5.5L9.5 0H4z"/><path d="M9.5 0v4a1 1 0 0 0 1 1H14L9.5 0z"/></svg>';

  // Per-input state
  var state = {};

  function getState(inputId) {
    if (!state[inputId]) {
      state[inputId] = {
        prefix: "",
        listUrl: null,
        activeIndex: -1,
        items: [],
        debounceTimer: null
      };
    }
    return state[inputId];
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
      el.value = msg.value || "";
      // Scroll to end so filename is visible
      el.scrollLeft = el.scrollWidth;
      updatePrefixVisibility(msg.id);
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

  // Fetch directory listing from the registerDataObj endpoint
  function fetchListing(inputId, query) {
    var st = getState(inputId);
    if (!st.listUrl) return;

    var url = st.listUrl + "&path=" + encodeURIComponent(query);
    fetch(url)
      .then(function(resp) { return resp.json(); })
      .then(function(data) {
        st.items = data.items || [];
        st.queryBase = data.base || "";
        renderDropdown(inputId);
      })
      .catch(function() {
        st.items = [];
        renderDropdown(inputId);
      });
  }

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

    dropdown.innerHTML = html;

    // Move dropdown to document.body so it escapes any overflow:auto/hidden
    // ancestors (e.g. dockview panels). Position with fixed coords relative
    // to the input field.
    if (dropdown.parentElement !== document.body) {
      document.body.appendChild(dropdown);
    }
    var field = document.getElementById(inputId)
      ? document.getElementById(inputId).closest(".blockr-path-input-field")
      : null;
    if (field) {
      var rect = field.getBoundingClientRect();
      dropdown.style.position = "fixed";
      dropdown.style.top = rect.bottom + "px";
      dropdown.style.left = rect.left + "px";
      dropdown.style.right = "auto";
      dropdown.style.width = rect.width + "px";
    }
    dropdown.style.display = "block";

    // Attach click handlers
    var items = dropdown.querySelectorAll(".blockr-path-dropdown-item");
    for (var j = 0; j < items.length; j++) {
      (function(idx) {
        items[idx].addEventListener("mousedown", function(e) {
          e.preventDefault();
          selectItem(inputId, idx);
        });
      })(j);
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

  // Select an item from the dropdown
  function selectItem(inputId, idx) {
    var st = getState(inputId);
    var item = st.items[idx];
    if (!item) return;

    var input = document.getElementById(inputId);
    if (!input) return;

    var base = st.queryBase || getBase(input.value);

    if (item.isdir) {
      // Folder: set base + name + "/" and re-query
      input.value = base + item.name + "/";
      $(input).trigger("change");
      // Re-query with new path
      clearTimeout(st.debounceTimer);
      st.debounceTimer = setTimeout(function() {
        fetchListing(inputId, input.value);
      }, 100);
    } else {
      // File: set base + name and close dropdown
      input.value = base + item.name;
      $(input).trigger("change");
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

  // Initialize path inputs on document ready and after Shiny re-renders
  function initPathInputs() {
    var inputs = document.querySelectorAll(".blockr-path-text");
    inputs.forEach(function(input) {
      if (input.dataset.blockrInit) return;
      input.dataset.blockrInit = "true";

      var inputId = input.id;

      // Input change with debounce
      input.addEventListener("input", function() {
        updatePrefixVisibility(inputId);
        var st = getState(inputId);
        clearTimeout(st.debounceTimer);
        st.debounceTimer = setTimeout(function() {
          fetchListing(inputId, input.value);
        }, 250);
      });

      // Focus: trigger listing if URL available
      input.addEventListener("focus", function() {
        input.scrollLeft = 0;
        var st = getState(inputId);
        if (st.listUrl) {
          fetchListing(inputId, input.value);
        }
      });

      // Blur: scroll to end so filename is visible, then close dropdown
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

      // Keyboard navigation
      input.addEventListener("keydown", function(e) {
        var st = getState(inputId);
        var dropdown = document.getElementById(inputId + "_dropdown");
        if (!dropdown || dropdown.style.display === "none") return;

        if (e.key === "ArrowDown") {
          e.preventDefault();
          st.activeIndex = Math.min(st.activeIndex + 1, st.items.length - 1);
          renderDropdown(inputId);
        } else if (e.key === "ArrowUp") {
          e.preventDefault();
          st.activeIndex = Math.max(st.activeIndex - 1, 0);
          renderDropdown(inputId);
        } else if (e.key === "Enter") {
          e.preventDefault();
          if (st.activeIndex >= 0) {
            selectItem(inputId, st.activeIndex);
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
