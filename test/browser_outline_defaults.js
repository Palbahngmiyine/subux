// Run against a default docs build: page-0 contains headings named after each
// control below; page-1 contains sixteen headings named Section 0 through 15.
async (currentPage, { origin = 'http://127.0.0.1:4176' } = {}) => {
  const context = await currentPage.context().browser().newContext({
    viewport: { width: 844, height: 390 }, reducedMotion: 'reduce',
  });
  context.setDefaultTimeout(10000);
  const page = await context.newPage();
  try {
    await page.goto(origin + '/contents/page-0/');
    const controls = ['jt-main-nav', 'jt-menu-toggle', 'jt-sidebar',
      'jt-sidebar-toggle', 'jt-sidebar-backdrop', 'jt-outline', 'jt-outline-toggle'];
    for (const id of controls) {
      if (await page.locator('#' + id).count() !== 1 ||
          await page.locator('h2#' + id + '-1').count() !== 1) {
        throw new Error('Heading shadows template control: ' + id);
      }
      if (await page.locator('.jt-outline a[href="#' + id + '-1"]').count() !== 1) {
        throw new Error('Outline link misses renamed heading: ' + id);
      }
    }
    const outline = page.locator('aside#jt-outline');
    if (await outline.getAttribute('data-ready') !== 'true' || await outline.isVisible()) {
      throw new Error('Runtime selected a heading instead of the outline');
    }
    await page.locator('#jt-outline-toggle').click();
    if (!await outline.isVisible()) throw new Error('Outline toggle did not open the aside');
    await outline.locator('a[href="#jt-outline-1"]').click();
    if (new URL(page.url()).hash !== '#jt-outline-1' || await outline.isVisible()) {
      throw new Error('Renamed heading link failed to navigate or close the outline');
    }
    await page.goto(origin + '/contents/page-0/');
    await page.locator('#jt-sidebar-toggle').click();
    if (await page.locator('aside#jt-sidebar').getAttribute('data-open') !== 'true') {
      throw new Error('Heading collision broke the sidebar');
    }
    await page.keyboard.press('Escape');
    const viewports = [];
    for (const viewport of [{ width: 844, height: 390 }, { width: 390, height: 900 },
      { width: 1100, height: 700 }]) {
      await page.setViewportSize(viewport);
      await page.goto(origin + '/contents/page-1/');
      const toggle = page.locator('#jt-outline-toggle');
      await toggle.click();
      const bounds = await outline.boundingBox();
      const dimensions = await outline.evaluate(el => ({
        overflow: getComputedStyle(el).overflowY,
        client: el.clientHeight, scroll: el.scrollHeight,
      }));
      if (!bounds || bounds.y + bounds.height > viewport.height ||
          dimensions.overflow !== 'auto' || dimensions.scroll <= dimensions.client) {
        throw new Error('Long outline is not viewport-bounded and scrollable: ' +
          JSON.stringify({ bounds, dimensions, viewport }));
      }
      await outline.hover();
      await page.mouse.wheel(0, 2000);
      await page.waitForFunction(() => document.querySelector('aside#jt-outline').scrollTop > 0);
      const last = outline.locator('a[href="#section-15"]');
      const lastBounds = await last.boundingBox();
      if (!lastBounds || lastBounds.y < bounds.y ||
          lastBounds.y + lastBounds.height > bounds.y + bounds.height) {
        throw new Error('Last outline link is unreachable');
      }
      const scrollTop = await outline.evaluate(el => el.scrollTop);
      await last.click();
      if (new URL(page.url()).hash !== '#section-15' || await outline.isVisible() ||
          await toggle.getAttribute('aria-expanded') !== 'false') {
        throw new Error('Last link failed to navigate or close the outline');
      }
      viewports.push({ ...viewport, top: bounds.y, height: bounds.height, scrollTop });
    }
    await page.setViewportSize({ width: 1280, height: 900 });
    await page.goto(origin + '/contents/page-1/');
    if (!await outline.isVisible() || await page.locator('#jt-outline-toggle').isVisible() ||
        await outline.evaluate(el => getComputedStyle(el).position) !== 'sticky') {
      throw new Error('Desktop outline no longer uses its static sidebar layout');
    }
    return { controlsUnique: true, renamedLinksWork: true, sidebarWorks: true,
      viewports, desktopOutline: true };
  } finally { await context.close(); }
}
