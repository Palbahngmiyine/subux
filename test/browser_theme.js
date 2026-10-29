// Compare OS-dark rendering against both originals at the acceptance viewports.
// just-tips follows the OS; the original subux.dev remains light.
async page => {
  const browser=await page.context().browser().browserType().launch({
    channel:'chrome',headless:false,
    args:['--disable-gpu-rasterization','--force-color-profile=srgb','--blink-settings=imageAnimationPolicy=2'],
  });
  const context=await browser.newContext({viewport:{width:1280,height:900},colorScheme:'dark',reducedMotion:'reduce'});
  context.setDefaultTimeout(30000);
  context.setDefaultNavigationTimeout(60000);
  const p=await context.newPage();
  const report=[];
  try {
    for(const width of[1280,390]) {
      await p.setViewportSize({width,height:900});
      for(const [name,ports,path]of[['tips',[4175,4173],''],['tips-doc',[4175,4173],'contents/common-linux/'],['blog',[4177,4174],'']]) {
        for(const [i,port]of ports.entries()) {
          await p.goto(`http://127.0.0.1:${port}/${port===4175 ? path.replace(/\/$/, '') : path}`);
          await p.locator('h1').waitFor();
          if (name === 'tips-doc' && !(await p.locator('h1').textContent()).includes('일반적인 리눅스 설정 명령어 모음'))
            throw new Error('Reference document route did not resolve');
          await p.evaluate(()=>document.fonts.ready);
          if(i===1)await p.waitForFunction(()=>document.documentElement.dataset.profileReady==='true');
          await p.waitForTimeout(600);await p.mouse.move(0,0);
          await p.screenshot({path:`output/playwright/dark-${name}-${i?'subux':'reference'}-${width}.png`,animations:'disabled'});
          report.push({name,width,original:i===0,theme:await p.evaluate(()=>({class:document.documentElement.className,data:document.documentElement.dataset.theme,bg:getComputedStyle(document.body).backgroundColor}))});
        }
      }
    }
    return report;
  }finally{await context.close();await browser.close();}
}
