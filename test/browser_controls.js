// Compare real mobile navigation/sidebar states, including page scroll locking.
async page => {
  const browser = await page.context().browser().browserType().launch({
    channel: 'chrome', headless: false,
    args: ['--disable-gpu-rasterization', '--force-color-profile=srgb', '--blink-settings=imageAnimationPolicy=2'],
  });
  const report = [];
  try {
    for (const colorScheme of ['light', 'dark']) {
      for (const state of ['navigation', 'sidebar']) {
        const context = await browser.newContext({ viewport: { width: 390, height: 900 }, colorScheme, reducedMotion: 'reduce' });
        try {
          const files = [];
          for (const port of [4175, 4173]) {
            const p = await context.newPage();
            await p.bringToFront();
            const path = state === 'navigation' ? '/' : `/contents/common-linux${port === 4173 ? '/' : ''}`;
            await p.goto(`http://127.0.0.1:${port}${path}`);
            await p.evaluate(() => document.fonts.ready);
            if (port === 4173) await p.waitForFunction(() => document.documentElement.dataset.profileReady === 'true');
            const button = p.getByRole('button', {
              name: state === 'sidebar' ? 'Menu' : port === 4175 ? 'mobile navigation' : 'Toggle navigation', exact: true,
            });
            await button.click();
            await p.mouse.move(-10, -10);
            await p.waitForTimeout(1000);
            const locked = await p.evaluate(() => ({ y: scrollY, width: document.documentElement.clientWidth }));
            if (locked.width !== 390) throw new Error(`${port}/${state}: root scrollbar did not close`);
            await p.mouse.move(380, 500);
            await p.mouse.wheel(0, 500);
            await p.waitForTimeout(200);
            if (await p.evaluate(() => scrollY) !== locked.y) throw new Error(`${port}/${state}: background still scrolls`);
            await p.mouse.move(-10, -10);
            const file = `output/playwright/controls-${colorScheme}-${state}-${port}.png`;
            await p.screenshot({ path: file, animations: 'disabled' });
            files.push(file);
            if (state === 'sidebar') await p.mouse.click(380, 500);
            else await button.click();
            await p.waitForTimeout(700);
            if (state === 'sidebar') {
              if (await p.evaluate(() => document.documentElement.clientWidth) !== 375)
                throw new Error(`${port}: root scrollbar was not restored`);
              await p.mouse.move(380, 500);
              await p.mouse.wheel(0, 200);
              await p.waitForTimeout(200);
              if (await p.evaluate(() => scrollY) === locked.y) throw new Error(`${port}: scrolling was not restored`);
            }
            await p.close();
          }
          report.push({ colorScheme, state, files });
        } finally { await context.close(); }
      }
    }
    return report;
  } finally { await browser.close(); }
}
