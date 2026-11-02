// Verify the capture environment freezes GIFs while normal serving still animates.
// Browser settings apply equally to both original and generated documents.
async page => {
  const report = [];
  for (const policy of [0, 2]) {
    const browser = await page.context().browser().browserType().launch({
      channel: 'chrome', headless: false,
      args: ['--disable-gpu-rasterization', '--force-color-profile=srgb', `--blink-settings=imageAnimationPolicy=${policy}`],
    });
    try {
      const context = await browser.newContext({ viewport: { width: 1280, height: 900 }, colorScheme: 'light', reducedMotion: 'reduce' });
      for (const port of [4177, 4174]) {
        const p = await context.newPage();
        await p.bringToFront();
        await p.goto(`http://127.0.0.1:${port}/effective-harnesses-for-long-running-agents/`);
        await p.evaluate(() => document.fonts.ready);
        if (port === 4174) await p.waitForFunction(() => document.documentElement.dataset.profileReady === 'true');
        const image = p.locator('img[src$=".gif"]');
        await image.evaluate(image => image.decode());
        await image.scrollIntoViewIfNeeded();
        await p.mouse.move(-10, -10);
        await p.waitForTimeout(700);
        const firstPath = `output/playwright/image-policy-${policy}-${port}-before.png`;
        const lastPath = `output/playwright/image-policy-${policy}-${port}-after.png`;
        const first = await p.screenshot({ path: firstPath, animations: 'disabled' });
        // This original GIF advances every 300 centiseconds.
        await p.waitForTimeout(3500);
        const last = await p.screenshot({ path: lastPath, animations: 'disabled' });
        const identical = first.equals(last);
        if (identical !== (policy === 2))
          throw new Error(`${port}: GIF policy ${policy} did not produce the expected frame timing`);
        report.push({ port, policy, identical, files: [firstPath, lastPath] });
        await p.close();
      }
      await context.close();
    } finally { await browser.close(); }
  }
  return report;
}
