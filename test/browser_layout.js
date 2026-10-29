// Run with the Playwright browser_run_code tool (filename), against the three
// running servers documented in STYLE-VERIFICATION.md. Baselines are originals.
async (currentPage) => {
  const context = await currentPage.context().browser().newContext({ viewport: { width: 1280, height: 900 }, colorScheme: 'light', reducedMotion: 'reduce' });
  const page = await context.newPage();
  try {

    const report = [];
    const targets = [
      ['tips-reference', 'http://127.0.0.1:4175/'],
      ['tips-subux', 'http://127.0.0.1:4173/'],
      ['tips-doc-reference', 'http://127.0.0.1:4175/contents/common-linux'],
      ['tips-doc-subux', 'http://127.0.0.1:4173/contents/common-linux/'],
      ['blog-reference', 'http://127.0.0.1:4177/'],
      ['blog-subux', 'http://127.0.0.1:4174/'],
    ];
    for (const width of [1280, 390]) {
      await page.setViewportSize({ width, height: 900 });
      await page.emulateMedia({ colorScheme: 'light', reducedMotion: 'reduce' });
      for (const [name, url] of targets) {
        await page.goto(url);
        await page.locator('h1').waitFor({ state: 'visible' });
        await page.evaluate(() => document.fonts.ready);
        if (await page.locator('script[src="/assets/custom.js"]').count()) {
          await page.waitForFunction(() => document.documentElement.dataset.profileReady);
          if (await page.evaluate(() => document.documentElement.dataset.profileReady) !== 'true')
            throw new Error(`${name}: profile failed`);
        }
        // Await fonts introduced by the optional highlighter and allow initial
        // paints to settle equally on the original and generated sites.
        await page.evaluate(() => document.fonts.ready);
        await page.waitForTimeout(200);
        await page.mouse.move(0, 0);
        await page.screenshot({ animations: 'disabled', path: `output/playwright/${name}-${width}.png` });
        const layout = await page.evaluate(() => ({
          overflow: document.documentElement.scrollWidth > innerWidth,
          heading: document.querySelector('h1').getBoundingClientRect().toJSON(),
        }));
        if (layout.overflow) throw new Error(`${name}: overflow at ${width}`);
        report.push({ name, width, ...layout });
      }
    }
    // Separate default-theme fixture keeps search verification independent of
    // just-tips, whose original configuration does not enable search.
    await page.goto('http://127.0.0.1:4176/');
    await page.getByRole('button', { name: 'Toggle navigation' }).click();
    await page.getByRole('button', { name: 'Search documentation' }).click();
    await page.getByRole('searchbox').fill('깃');
    await page.getByRole('link', { name: '깃 설정 팁', exact: true }).waitFor();
    await page.keyboard.press('Escape');
    const focusRestored = await page.getByRole('button', { name: 'Search documentation' })
      .evaluate(el => el === document.activeElement);
    if (!focusRestored) throw new Error('Search did not restore focus');
    await page.getByRole('link', { name: '문서로 바로가기' }).click();
    await page.getByRole('heading', { name: '일반적인 리눅스 설정 명령어 모음' }).waitFor();
    report.push({ searchFocusRestored: true, linuxPage: true });
    return report;
  } finally { await context.close(); }
}
