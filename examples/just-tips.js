// Optional client highlighting for the VitePress 1.6 profile. Markdown stays
// readable with the native highlighter if the CDN is unavailable.
const blocks = [...document.querySelectorAll('.jt-doc-main pre > code')];
try {
  if (blocks.length) {
    const { codeToHtml } = await import('https://esm.sh/shiki@2.5.0/bundle/web');
    for (const code of blocks) {
      const lang = [...code.classList].find(name => name.startsWith('language-'))?.slice(9) || 'text';
      const html = await codeToHtml(code.textContent.replace(/\n$/, ''), {
        lang, themes: { light: 'github-light', dark: 'github-dark' }, defaultColor: false,
      });
      const template = document.createElement('template');
      template.innerHTML = html;
      code.replaceWith(template.content.querySelector('code'));
    }
  }
  document.documentElement.dataset.profileReady = 'true';
} catch (error) {
  document.documentElement.dataset.profileReady = 'failed';
  console.error('Optional just-tips highlighting failed', error);
}

// The source outline keeps the current section visible while the page scrolls.
const outline = document.querySelector('.jt-outline');
if (outline) {
  const placeholder = document.createElement('div');
  placeholder.className = 'jt-outline-placeholder';
  outline.replaceWith(placeholder);
  placeholder.append(outline);
  const links = [...outline.querySelectorAll('a[href^="#"]:not(.jt-outline-top)')];
  const entries = links.map(link => ({ link, heading: document.getElementById(decodeURIComponent(link.hash.slice(1))) })).filter(entry => entry.heading);
  const marker = document.createElement('span');
  marker.className = 'jt-outline-marker';
  marker.setAttribute('aria-hidden', 'true');
  outline.append(marker);
  const updateOutline = () => {
    let active = null;
    if (matchMedia('(min-width: 1280px)').matches && scrollY > 0) {
      if (Math.abs(scrollY + innerHeight - document.body.offsetHeight) < 1) active = entries.at(-1);
      else for (const entry of entries) {
        const offset = parseFloat(getComputedStyle(document.documentElement).getPropertyValue('--sx-profile-scroll-offset'));
        if (entry.heading.getBoundingClientRect().top > offset + 4) break;
        active = entry;
      }
    }
    for (const { link } of entries) {
      if (link === active?.link) link.setAttribute('aria-current', 'location');
      else link.removeAttribute('aria-current');
    }
    marker.hidden = !active;
    if (active) marker.style.top = `${active.link.offsetTop + 7}px`;
  };
  addEventListener('scroll', updateOutline, { passive: true });
  addEventListener('resize', updateOutline);
  updateOutline();
}

// Preserve the local navigation's flow space when it pins to the viewport.
const localNav = document.querySelector('.jt-local-nav');
if (localNav) {
  const position = document.createElement('div');
  position.className = 'jt-local-nav-position';
  localNav.replaceWith(position);
  position.append(localNav);
  const updateLocalNav = () => {
    // Keep the dropdown in the navigation's paint layer, as in the source.
    // Return it to its desktop flow placeholder when the outline is inline.
    if (outline) {
      const parent = matchMedia('(max-width: 1279px)').matches
        ? localNav : document.querySelector('.jt-outline-placeholder');
      if (parent && outline.parentElement !== parent) parent.append(outline);
    }
    const offset = position.getBoundingClientRect().top;
    localNav.classList.toggle('jt-is-pinned', matchMedia('(max-width: 959px)').matches && offset <= 0);
    document.documentElement.style.setProperty('--sx-profile-local-nav-offset', `${Math.max(0, offset)}px`);
  };
  addEventListener('scroll', updateLocalNav, { passive: true });
  addEventListener('resize', updateLocalNav);
  updateLocalNav();
}
