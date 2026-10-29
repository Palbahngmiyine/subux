// Real interactions on generated pages; no source styles or screenshots are replaced.
async (currentPage) => {
  const context = await currentPage.context().browser().newContext({ viewport: { width: 1280, height: 900 }, colorScheme: 'light', reducedMotion: 'reduce' });
  const page = await context.newPage();
  try {

    const origin = 'http://127.0.0.1:4173';
    await page.context().grantPermissions(['clipboard-read', 'clipboard-write'], { origin });
    await page.setViewportSize({ width: 390, height: 900 });
    await page.emulateMedia({ colorScheme: 'light', reducedMotion: 'reduce' });
    await page.goto(origin);
    await page.evaluate(() => localStorage.clear());
    await page.reload();
    const menu = page.getByRole('button', { name: 'Toggle navigation' });
    await menu.click();
    await page.getByRole('link', { name: '설정 일람', exact: true }).waitFor({ state: 'visible' });
    if (await page.locator('body').evaluate(el => el.style.overflow) !== 'hidden')
      throw new Error('Mobile navigation does not lock page scrolling');
    const theme = page.getByRole('switch', { name: 'Toggle color theme' });
    await theme.click();
    if (await theme.getAttribute('aria-checked') !== 'true') throw new Error('Theme state not announced');
    await page.reload();
    if (await page.locator('html').getAttribute('data-theme') !== 'dark') throw new Error('Theme not persisted');
    await menu.click();
    await theme.click();
    await page.keyboard.press('Escape');
    if (!await menu.evaluate(el => el === document.activeElement)) throw new Error('Menu focus not restored');
    await page.goto(`${origin}/contents/common-linux/`);
    await page.waitForFunction(() => document.documentElement.dataset.profileReady === 'true');
    const sidebar = page.getByRole('button', { name: 'Menu', exact: true });
    await sidebar.click();
    const links = page.locator('#jt-sidebar a');
    await page.keyboard.press('Shift+Tab');
    if (!await links.last().evaluate(el => el === document.activeElement)) throw new Error('Sidebar focus escaped');
    await page.keyboard.press('Escape');
    if (!await sidebar.evaluate(el => el === document.activeElement)) throw new Error('Sidebar focus not restored');
    await sidebar.click();
    await page.locator('#jt-sidebar-backdrop').click({ position: { x: 350, y: 400 } });
    if (await sidebar.getAttribute('aria-expanded') !== 'false') throw new Error('Backdrop did not close sidebar');
    const outline = page.getByRole('button', { name: 'On this page', exact: true });
    await outline.click();
    await page.getByRole('link', { name: 'Return to top' }).waitFor({ state: 'visible' });
    await page.mouse.click(10, 500);
    if (await outline.getAttribute('aria-expanded') !== 'false') throw new Error('Outside click did not close outline');
    await page.evaluate(() => scrollTo({ top: 750, behavior: 'instant' }));
    await outline.click();
    const scrolledTop = await page.getByRole('link', { name: 'Return to top', exact: true })
      .evaluate(el => el.getBoundingClientRect().top);
    if (scrolledTop !== 41) throw new Error('Scrolled outline is detached from the pinned navigation');
    await page.keyboard.press('Escape');
    await page.evaluate(() => scrollTo({ top: 0, behavior: 'instant' }));
    const code = page.locator('pre code').first();
    const expected = await code.textContent();
    await page.locator('.sx-code-copy').first().click();
    await page.waitForFunction(() => document.querySelector('.sx-code-copy').getAttribute('aria-label') === 'Copied');
    if (await page.evaluate(() => navigator.clipboard.readText()) !== expected) throw new Error('Copied code differs');
    await page.waitForFunction(() => !document.querySelector('.sx-code-copy').dataset.copied &&
      document.querySelector('.sx-code-copy').getAttribute('aria-label') === 'Copy code');
    return { copyFeedbackReset: true, mobileMenu: true, themePersistence: true, sidebarFocus: true, backdrop: true, outlineDismissal: true, clipboard: true };
  } finally { await context.close(); }
}
