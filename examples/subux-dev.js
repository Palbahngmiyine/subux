// Match the source site's rehype-highlight configuration, when opted in.
// The original has one light theme and renders plain code/table elements.
document.documentElement.dataset.theme = 'light';
for (const wrapper of document.querySelectorAll('.article-content .sx-code-block, .article-content .table-wrapper')) {
  const content = wrapper.querySelector('pre, table');
  if (content) wrapper.replaceWith(content);
}
const codes = [...document.querySelectorAll('.article-content pre > code')];
try {
  const plain = new Set(['text', 'txt', 'plain', 'mermaid', 'mmd']);
  const highlighted = codes.filter(code => {
    const lang = [...code.classList].find(name => name.startsWith('language-'))?.slice(9);
    return lang && !plain.has(lang);
  });
  if (highlighted.length) {
    const { default: hljs } = await import('https://esm.sh/highlight.js@11.11.1');
    hljs.registerAliases(['sh', 'shell', 'zsh'], { languageName: 'bash' });
    hljs.registerAliases(['yml'], { languageName: 'yaml' });
    for (const code of highlighted) {
      const language = [...code.classList].find(name => name.startsWith('language-')).slice(9);
      if (hljs.getLanguage(language)) {
        code.innerHTML = hljs.highlight(code.textContent, { language }).value;
        code.classList.add('hljs');
      }
    }
  }
  document.documentElement.dataset.profileReady = 'true';
} catch (error) {
  document.documentElement.dataset.profileReady = 'failed';
  console.error('Optional subux.dev highlighting failed', error);
}
