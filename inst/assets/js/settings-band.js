// @ts-check
/**
 * CANONICAL SOURCE: blockr.viz/inst/js/settings-band.js — keep in sync.
 * Copied verbatim into blockr.io per the design-system convention.
 * blockr.io currently uses only the CSS side of the band (its checkboxes
 * are Shiny-rendered); the Blockr.checkbox factory below is vendored for
 * sync parity, not (yet) called.
 *
 * settings-band.js — shared factory for the design-system boolean control
 * (see blockr.ui/dev/boolean-controls-proposals.html): a .blockr-checkbox
 * built on a native <input type="checkbox">.
 *
 * Staged in blockr.viz for the settings-band pilot; moves to blockr.ui
 * (blockr-core.js) with the shared layer. Loaded before drilldown-config.js /
 * the block scripts, which use Blockr.checkbox when present.
 */
(function () {
  'use strict';

  var CHECK_SVG =
    '<svg width="10" height="10" viewBox="0 0 16 16" fill="currentColor">' +
    '<path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 ' +
    '0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>';

  /**
   * Build a design-system checkbox.
   * @param {string} label
   * @param {boolean} checked
   * @param {(checked: boolean) => void} onChange
   * @returns {{ el: HTMLLabelElement, input: HTMLInputElement,
   *             set: (v: boolean) => void, get: () => boolean }}
   */
  function checkbox(label, checked, onChange) {
    var wrap = document.createElement('label');
    wrap.className = 'blockr-checkbox';
    var input = document.createElement('input');
    input.type = 'checkbox';
    input.checked = !!checked;
    var box = document.createElement('span');
    box.className = 'blockr-checkbox__box';
    box.innerHTML = CHECK_SVG;
    var txt = document.createElement('span');
    txt.className = 'blockr-checkbox__label';
    txt.textContent = label;
    input.addEventListener('change', function () { onChange(input.checked); });
    wrap.appendChild(input);
    wrap.appendChild(box);
    wrap.appendChild(txt);
    return {
      el: wrap,
      input: input,
      set: function (v) { input.checked = !!v; },
      get: function () { return input.checked; }
    };
  }

  var ns = /** @type {BlockrNamespace} */ (
    (typeof Blockr !== 'undefined') ? Blockr
      : (window.Blockr = window.Blockr || /** @type {BlockrNamespace} */ ({})));
  ns.checkbox = checkbox;
})();
