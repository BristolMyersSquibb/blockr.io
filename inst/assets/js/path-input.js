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

  // Custom message handler: update prefix text
  Shiny.addCustomMessageHandler("blockr-path-prefix", function(msg) {
    var el = document.getElementById(msg.id + "_prefix");
    if (el) {
      el.textContent = msg.prefix || "";
    }
    var st = getState(msg.id);
    st.prefix = msg.prefix || "";
  });

  // Custom message handler: store list_dir endpoint URL
  Shiny.addCustomMessageHandler("blockr-path-list-url", function(msg) {
    var st = getState(msg.id);
    st.listUrl = msg.url;
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

  // Select an item from the dropdown
  function selectItem(inputId, idx) {
    var st = getState(inputId);
    var item = st.items[idx];
    if (!item) return;

    var input = document.getElementById(inputId);
    if (!input) return;

    if (item.isdir) {
      // Folder: append name + "/" and re-query
      var currentVal = input.value;
      // Find the last "/" and replace everything after it with the folder name
      var lastSlash = currentVal.lastIndexOf("/");
      var base = lastSlash >= 0 ? currentVal.substring(0, lastSlash + 1) : "";
      input.value = base + item.name + "/";
      $(input).trigger("change");
      // Re-query with new path
      clearTimeout(st.debounceTimer);
      st.debounceTimer = setTimeout(function() {
        fetchListing(inputId, input.value);
      }, 100);
    } else {
      // File: set full name and close dropdown
      var currentVal2 = input.value;
      var lastSlash2 = currentVal2.lastIndexOf("/");
      var base2 = lastSlash2 >= 0 ? currentVal2.substring(0, lastSlash2 + 1) : "";
      input.value = base2 + item.name;
      $(input).trigger("change");
      closeDropdown(inputId);
    }
  }

  function closeDropdown(inputId) {
    var dropdown = document.getElementById(inputId + "_dropdown");
    if (dropdown) {
      dropdown.style.display = "none";
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
        var st = getState(inputId);
        clearTimeout(st.debounceTimer);
        st.debounceTimer = setTimeout(function() {
          fetchListing(inputId, input.value);
        }, 250);
      });

      // Focus: trigger listing if URL available
      input.addEventListener("focus", function() {
        var st = getState(inputId);
        if (st.listUrl) {
          fetchListing(inputId, input.value);
        }
      });

      // Blur: close dropdown (with small delay to allow click)
      input.addEventListener("blur", function() {
        setTimeout(function() {
          closeDropdown(inputId);
        }, 200);
      });

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
