// @ts-check
/**
 * io-blocks.js — blockr.io block helpers.
 *
 * Naming: the blockr- prefix is reserved for the shared design system;
 * io-local classes use io-. blockrIoGearToggle keeps its historical name
 * (it is wired into the blocks' inline onclick handlers).
 */
(function () {
  'use strict';

  if (window.blockrIoGearToggle) return;

  /**
   * Toggle the settings band opened by a gear button. The band is a
   * persistent in-flow panel (role="region"), not a menu: there is no
   * outside-click closer. Visibility is class-driven — closed means no
   * `blockr-settings--open` (settings-band.css).
   *
   * @param {string} gearId id of the .blockr-gear-btn
   * @param {string} bandId id of the .blockr-settings band
   */
  window.blockrIoGearToggle = function (gearId, bandId) {
    var gear = document.getElementById(gearId);
    var band = document.getElementById(bandId);
    if (!gear || !band) return;
    var open = band.classList.toggle('blockr-settings--open');
    gear.classList.toggle('blockr-gear-active', open);
    gear.setAttribute('aria-expanded', open ? 'true' : 'false');
  };
})();
