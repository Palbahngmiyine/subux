// Compare the open outline at the top, during header scroll, and when pinned.
async page => {
  const report = [];
  for (const colorScheme of ['light', 'dark']) {
    const context = await page.context().browser().newContext({
      viewport: { width: 390, height: 900 }, colorScheme, reducedMotion: 'reduce',
    });
    try {
      const pages = await Promise.all([context.newPage(), context.newPage()]);
      for (const [i, p] of pages.entries()) {
        await p.goto(`http://127.0.0.1:${i ? 4173 : 4175}/contents/common-linux${i ? '/' : ''}`);
        await p.locator('h1').filter({ hasText: '일반적인 리눅스 설정 명령어 모음' }).waitFor();
        await p.evaluate(() => document.fonts.ready);
        if (i) await p.waitForFunction(() => document.documentElement.dataset.profileReady === 'true');
      }
      for (const y of [0, 32, 750]) {
        const files = [];
        for (const [i, p] of pages.entries()) {
          await p.bringToFront();
          await p.evaluate(y => scrollTo({ top: y, behavior: 'instant' }), y);
          const button = p.locator(i ? '#jt-outline-toggle' : '.VPLocalNavOutlineDropdown button');
          await button.click();
          await p.mouse.move(0, 0);
          await p.waitForTimeout(650);
          const path = `output/playwright/outline-${colorScheme}-${y}-${i ? 'subux' : 'reference'}.png`;
          await p.screenshot({ path, animations: 'disabled' });
          files.push(path);
          // Click the toggle again: the source dropdown does not handle Escape.
          await button.click();
        }
        report.push({ colorScheme, y, files });
      }
      const generated = pages[1];
      await generated.setViewportSize({ width: 1280, height: 900 });
      await generated.waitForFunction(() => document.querySelector('.jt-outline').parentElement.className === 'jt-outline-placeholder');
      await generated.setViewportSize({ width: 390, height: 900 });
      await generated.locator('.jt-local-nav > .jt-outline').waitFor({ state: 'attached' });
      await generated.locator('#jt-outline-toggle').click();
      await generated.keyboard.press('Escape');
      if (await generated.locator('#jt-outline-toggle').getAttribute('aria-expanded') !== 'false')
        throw new Error('Resized outline did not close with Escape');
    } finally { await context.close(); }
  }
  return report;
}
