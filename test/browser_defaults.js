// Run against a default docs build containing a leading "Page title" H1,
// a "Back to title" fragment link, a table code span with two surrounding
// spaces, and at least eight documents matching "needle". No site CSS/JS.
async (currentPage, { origin = 'http://127.0.0.1:4176' } = {}) => {
  const context = await currentPage.context().browser().newContext({
    viewport: { width: 390, height: 900 }, colorScheme: 'light', reducedMotion: 'reduce',
  });
  context.setDefaultTimeout(30000);
  context.setDefaultNavigationTimeout(60000);
  const page = await context.newPage();
  try {
    const menus = [];
    for (const viewport of [{ width: 390, height: 900 }, { width: 844, height: 390 }]) {
      await page.setViewportSize(viewport);
      await page.goto(origin + '/');
      const menu = page.locator('#jt-main-nav');
      const headerFilter = await page.locator('.jt-navbar').evaluate(el => getComputedStyle(el).backdropFilter);
      if (await menu.isVisible()) throw new Error('Mobile navigation starts open');
      const toggle = page.getByRole('button', { name: 'Toggle navigation' });
      await toggle.click();
      const bounds = await menu.boundingBox();
      if (!bounds || Math.abs(bounds.y + bounds.height - viewport.height) > 1 ||
          bounds.height < viewport.height / 2) {
        throw new Error('Mobile navigation is constrained by its header: ' + JSON.stringify(bounds));
      }
      menus.push({ ...viewport, top: bounds.y, height: bounds.height });
      await page.keyboard.press('Escape');
      if (await menu.isVisible()) throw new Error('Escape did not close navigation');
      if (!await toggle.evaluate(el => el === document.activeElement)) throw new Error('Menu focus not restored');
      if (await page.locator('.jt-navbar').evaluate(el => getComputedStyle(el).backdropFilter) !== headerFilter) {
        throw new Error('Closing navigation changed the header filter');
      }
    }
    await page.setViewportSize({ width: 1280, height: 900 });
    if (!await page.locator('#jt-main-nav').isVisible()) throw new Error('Desktop navigation disappeared');
    if (await page.getByRole('button', { name: 'Toggle navigation' }).isVisible()) {
      throw new Error('Desktop navigation shows a mobile toggle');
    }
    if (await page.locator('#page-title').count() !== 1 ||
        await page.locator('#page-title').evaluate(el => el.tagName) !== 'H1') {
      throw new Error('Removed Markdown title anchor is missing or duplicated');
    }
    await page.getByRole('link', { name: 'Back to title', exact: true }).click();
    if (new URL(page.url()).hash !== '#page-title') throw new Error('Title fragment navigation failed');
    if (await page.locator('td code').first().textContent() !== ' foo ') {
      throw new Error('Table code lost significant surrounding spaces');
    }
    await page.goto(origin + '/contents/page-0/');
    const outlineColors = [];
    for (const colorScheme of ['light', 'dark']) {
      await page.emulateMedia({ colorScheme });
      const colors = await page.locator('.jt-outline li a').first().evaluate(link => {
        const probe = document.createElement('span');
        probe.style.cssText = 'position:fixed;visibility:hidden;color:var(--sx-color-link)';
        document.body.append(probe);
        const actual = getComputedStyle(link);
        const result = { color: actual.color, token: getComputedStyle(probe).color,
          height: link.getBoundingClientRect().height,
          decoration: actual.textDecorationLine };
        probe.remove();
        return result;
      });
      if (colors.color !== colors.token || !colors.decoration.includes('underline') || colors.height < 44) {
        throw new Error('Outline lost its link treatment: ' + JSON.stringify(colors));
      }
      outlineColors.push({ colorScheme, ...colors });
    }
    await page.setViewportSize({ width: 844, height: 390 });
    await page.keyboard.press('Control+k');
    await page.getByRole('searchbox').fill('needle');
    const last = page.locator('#jt-search-results a').nth(7);
    await last.waitFor({ state: 'attached' });
    await page.mouse.move(400, 200);
    await page.mouse.wheel(0, 1000);
    await page.waitForFunction(() => {
      const modal = document.getElementById('jt-search-modal');
      const last = modal.querySelector('li:last-child');
      const box = last.getBoundingClientRect();
      return modal.scrollTop > 0 && box.top >= 0 && box.bottom <= innerHeight + 1;
    });
    const searchScrollTop = await page.locator('#jt-search-modal').evaluate(el => el.scrollTop);
    const href = await last.getAttribute('href');
    await last.click();
    await page.waitForURL(new URL(href, origin).href);
    return { menus, titleAnchor: true, tableSpaces: true, outlineColors, searchScrollTop,
      lastResultNavigable: true };
  } finally { await context.close(); }
}
