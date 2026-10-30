// Capture actual viewport pixels throughout each page. Chrome's fullPage capture
// can remove classic scrollbars and reflow content; these captures retain them.
// Invoke once with the current page viewport set to 1280x900, then at 390x900.
// Progress remains recoverable if a tool response expires during a long run.
async (page, { sites = null, colorScheme = 'light', controlled = false, captureTag = '' } = {}) => {
  const routes = [
    ['tips', 'http://127.0.0.1:4175/', 'http://127.0.0.1:4173/'],
    ['tips-linux', 'http://127.0.0.1:4175/contents/common-linux', 'http://127.0.0.1:4173/contents/common-linux/'],
    ['tips-git', 'http://127.0.0.1:4175/contents/git', 'http://127.0.0.1:4173/contents/git/'],
    ...['', 'tech/', 'translations/', '2026/PAPERBOOK-AND-MARKDOWN/', '2026/TEAM-AND-LEADERSHIP/',
      '2025/WRITING-AND-AI/', '2026/KUBERNETES-POD-NETWORKING-PART-1/',
      '2026/KUBERNETES-POD-NETWORKING-PART-2/', 'effective-harnesses-for-long-running-agents/',
      'how-complex-systems-fail/', 'effective-context-engineering-for-ai-agents/',
      'two-pizza-team/', 'rob-pikes-5-rules-of-programming/', 'harness-design-long-running-apps/']
      .map((path, i) => [`blog-${i}`, `http://127.0.0.1:4177/${path === 'tech/' ? '?tab=tech' : path === 'translations/' ? '?tab=translations' : path}`, `http://127.0.0.1:4174/${path}`]),
  ];
  const selectedRoutes = sites === null ? routes : routes.filter(([name]) => sites.includes(name));
  if (!selectedRoutes.length || (sites !== null && selectedRoutes.length !== new Set(sites).size))
    throw new Error('Unknown or empty site selection');
  const report = [];
  const width = page.viewportSize().width;
  if (![1280, 390].includes(width)) throw new Error('Set viewport width to 1280 or 390 before running');
  if (!/^[a-z0-9-]{0,32}$/.test(captureTag)) throw new Error('Invalid capture tag');
  let ownedBrowser = null;
  let browser = page.context().browser();
  let environment = { mode: 'default', browserVersion: browser.version() };
  // Parent-page memory makes a timed-out tool response recoverable without
  // restarting a browser run. Captured source/output documents are untouched.
  await page.evaluate(width => {
    window.__subuxScrollRuns ??= {};
    window.__subuxScrollRuns[width] = { status: 'running', report: [] };
  }, width);
  try {
    if (controlled) {
      const args = ['--disable-gpu-rasterization', '--force-color-profile=srgb', '--blink-settings=imageAnimationPolicy=2'];
      ownedBrowser = await browser.browserType().launch({ channel: 'chrome', headless: false, args });
      browser = ownedBrowser;
      const session = await browser.newBrowserCDPSession();
      const info = await session.send('SystemInfo.getInfo');
      await session.detach();
      if (info.gpu.featureStatus.rasterization !== 'disabled_software' || !args.every(arg => info.commandLine.includes(arg)))
        throw new Error('Controlled renderer flags were not applied');
      environment = { mode: 'cpu-srgb-frozen-images', browserVersion: browser.version(), rasterization: info.gpu.featureStatus.rasterization, args };
    }
    await page.evaluate(({ width, environment, captureTag }) => {
      Object.assign(window.__subuxScrollRuns[width], { environment, captureTag });
    }, { width, environment, captureTag });
    for (const [name, ...urls] of selectedRoutes) {
      const context = await browser.newContext({ viewport: { width, height: 900 }, colorScheme, reducedMotion: 'reduce' });
      const pages = await Promise.all(urls.map(() => context.newPage()));
      try {
        for (const [i, p] of pages.entries()) {
          p.setDefaultTimeout(30000);
          p.setDefaultNavigationTimeout(60000);
          await p.bringToFront();
          const response = await p.goto(urls[i]);
          if (!response.ok()) throw new Error(`${urls[i]}: ${response.status()}`);
          await p.locator('h1').waitFor();
          await p.evaluate(() => document.fonts.ready);
          if (i === 1) await p.waitForFunction(() => document.documentElement.dataset.profileReady === 'true');
          // Native lazy images load when scrolled into view. Decoding all of
          // them before scrolling would wait forever on off-screen images.
          for (const image of await p.locator('img').all()) {
            await image.scrollIntoViewIfNeeded();
            await image.evaluate(image => Promise.race([
              image.decode(), new Promise((_, reject) => setTimeout(() => reject(new Error('Image decode timed out')), 30000)),
            ]));
          }
          for (const video of await p.locator('video').all()) {
            await video.scrollIntoViewIfNeeded();
            // Load through real playback, then rewind. Partial network buffers
            // otherwise give native progress controls different painted widths.
            await video.evaluate(async video => { video.playbackRate = 16; await video.play(); });
            await p.waitForFunction(video => video.ended, await video.elementHandle(), { timeout: 45000 });
            await video.evaluate(async video => {
              video.pause();
              video.playbackRate = 1;
              await new Promise(resolve => {
                video.addEventListener('seeked', resolve, { once: true });
                video.currentTime = 0;
              });
              if (video.buffered.length !== 1 || video.buffered.start(0) !== 0 || video.buffered.end(0) < video.duration - 0.01)
                throw new Error('Video is not fully buffered');
            });
          }
          await p.evaluate(() => scrollTo({ top: 0, behavior: 'instant' }));
          await p.evaluate(() => document.fonts.ready);
          await p.mouse.move(-10, -10);
          await p.waitForTimeout(200);
        }
        const heights = await Promise.all(pages.map(p => p.evaluate(() => document.documentElement.scrollHeight)));
        if (heights[0] !== heights[1]) throw new Error(`${name}/${width}: heights ${heights}`);
        const last = Math.max(0, heights[0] - 900);
        const offsets = [];
        for (let y = 0; y < last; y += 750) offsets.push(y);
        offsets.push(last);
        for (const y of offsets) {
          const files = [];
          const positions = [];
          for (const [i, p] of pages.entries()) {
            await p.bringToFront();
            // Activating a headed tab can restore the native pointer position.
            // Align input after activation, before waiting for hover transitions.
            await p.mouse.move(1, 1);
            await p.mouse.move(-10, -10);
            await p.evaluate(y => scrollTo({ top: y, behavior: 'instant' }), y);
            // Source scroll listeners debounce for 100ms and color transitions last 500ms.
            await p.waitForTimeout(650);
            // Native activation may finish after the first injected move.
            await p.mouse.move(1, 1);
            await p.mouse.move(-10, -10);
            await p.waitForTimeout(300);
            const hovered = await p.locator(':is(a, button, pre, code):hover').evaluateAll(elements => elements.map(el => `${el.tagName}.${el.className}`));
            if (hovered.length)
              throw new Error(`${name}/${width}/${y}/${i ? 'subux' : 'reference'}: unexpected hover ${hovered.join(', ')}`);
            const before = await p.evaluate(() => ({ y: scrollY, width: innerWidth, client: document.documentElement.clientWidth }));
            const path = `output/playwright/scroll-${controlled ? 'controlled-' : ''}${captureTag ? `${captureTag}-` : ''}${colorScheme === 'dark' ? 'dark-' : ''}${name}-${width}-${y}-${i ? 'subux' : 'reference'}.png`;
            const png = await p.screenshot({ path, animations: 'disabled' });
            const after = await p.evaluate(() => ({ y: scrollY, width: innerWidth, client: document.documentElement.clientWidth }));
            if (JSON.stringify(before) !== JSON.stringify(after) || png.readUInt32BE(16) !== width || png.readUInt32BE(20) !== 900)
              throw new Error(`${name}/${width}/${y}: capture changed the viewport`);
            files.push(path);
            positions.push(before);
          }
          if (JSON.stringify(positions[0]) !== JSON.stringify(positions[1])) throw new Error(`${name}/${width}/${y}: viewport mismatch`);
          report.push({ name, width, colorScheme, y, height: heights[0], environment, files });
        }
      } finally { await context.close(); }
      await page.evaluate(({ width, report }) => {
        window.__subuxScrollRuns[width].report = report;
      }, { width, report });
    }
    await page.evaluate(width => { window.__subuxScrollRuns[width].status = 'complete'; }, width);
    return report;
  } catch (error) {
    await page.evaluate(({ width, error }) => {
      window.__subuxScrollRuns[width].status = 'failed';
      window.__subuxScrollRuns[width].error = error;
    }, { width, error: String(error) });
    throw error;
  } finally {
    if (ownedBrowser) await ownedBrowser.close();
  }
}
