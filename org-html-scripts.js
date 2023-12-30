(function (fn) {
  if (document && document.readyState === 'complete') {
    fn();
  } else {
    document.addEventListener('DOMContentLoaded', fn);
  }
})(function () {
  function CodeHighlightOn(elem, id) {
    var target = document.getElementById(id);
    if (null != target) {
      elem.classList.add('code-highlighted');
      target.classList.add('code-highlighted');
      target.scrollIntoView();
    }
  }
  function CodeHighlightOff(elem, id) {
    var target = document.getElementById(id);
    if (null != target) {
      elem.classList.remove('code-highlighted');
      target.classList.remove('code-highlighted');
      target.scrollIntoView();
    }
  }
  function toggleSideMenu() {
    document.querySelector('#table-of-contents').classList.toggle('open');
  }
  function initSideMenu() {
    const title = document.querySelector('#table-of-contents h2');
    if (title) {
      title.addEventListener('click', toggleSideMenu);
    }
  }
  function addHighlightRefs() {
    document.querySelectorAll('.coderef').forEach((el) => {
      const targetId =
        el.getAttribute('href') && el.getAttribute('href').substring(1);
      el.addEventListener('click', () => {
        CodeHighlightOn(el, targetId);
      });
      el.addEventListener('focusin', () => {
        CodeHighlightOn(el, targetId);
      });
      el.addEventListener('focusout', () => {
        CodeHighlightOff(el, targetId);
      });
    });
  }
  addHighlightRefs();
  const initDarkTheme = function () {
    const createToggleButton = function ({ onToggle, checked } = {}) {
      const moon = '&#127769';
      const sun = '🌞';
      const thumb = document.createElement('div');
      thumb.style.cssText = `position: absolute;
                             top: -1px;
                             left: 1px;
                             width: 22px;
                             height: 22px;
                             border-radius: 50%;
                             background-color: black;
                             border: 1px solid pink;
                             box-sizing: border-box;
                             transition: all .5s cubic-bezier(.23,1,.32,1) 0ms;
                             transform: translateX(0);`;
      const toggleButtonElem = document.createElement('button');
      const updateStyle = function (_el) {
        thumb.style['transform'] = checked
          ? `translateX(23px)`
          : `translateX(0px)`;
      };
      const handleClick = function ({ target }) {
        checked = !checked;
        updateStyle(target);
        if (onToggle) {
          onToggle(checked, target);
        }
      };
      toggleButtonElem.onclick = handleClick;
      toggleButtonElem.id = 'km-theme-toggler';
      toggleButtonElem.style.cssText = `position:absolute;
                                        z-index: 9999;
                                        right: 5px;
                                        cursor: pointer;
                                        top: 5px;
                                        transition: all 0.2s;
                                        border-color: none;
                                        background-color: black;
                                        width: 50px;
                                        height: 24px;
                                        padding: 0;
                                        border-radius: 30px;`;
      toggleButtonElem.innerHTML = `${moon}${sun}`;
      toggleButtonElem.appendChild(thumb);
      if (checked) {
        updateStyle(toggleButtonElem);
      }
      return toggleButtonElem;
    };
    const DARK_KEY = '__km-dark-theme';
    let darkStyle;
    function insertDarkTheme() {
      const css = `html {-webkit-filter: hue-rotate(180deg) invert(90%) !important;}
      iframe,img,video, pre, #table-of-contents, button  {-webkit-filter: brightness(90%) invert(100%) hue-rotate(180deg) !important;}`;
      const head = document.head || document.getElementsByTagName('head')[0];
      const style = document.createElement('style');
      head.appendChild(style);
      style.appendChild(document.createTextNode(css));
      return style;
    }
    const handleToggle = function (checked) {
      if (checked && !darkStyle) {
        darkStyle = insertDarkTheme();
        sessionStorage.setItem(DARK_KEY, 'true');
      } else {
        if (darkStyle) {
          darkStyle.remove();
        }
        darkStyle = null;
        sessionStorage.removeItem(DARK_KEY);
      }
    };
    const toggleButton = createToggleButton({
      onToggle: handleToggle,
      checked: sessionStorage.getItem(DARK_KEY),
    });
    if (sessionStorage.getItem(DARK_KEY)) {
      handleToggle(sessionStorage.getItem(DARK_KEY));
    }
    document.body.prepend(toggleButton);
  };
  function message(msg, time) {
    const createBox = function messageUnder(html) {
      let messageBox = document.createElement('div');
      const cross = document.createElement('span');
      cross.innerHTML = '&times';
      cross.onclick = () => messageBox.remove();
      cross.style.cssText = `margin-left: 15px;
color: white;
font-weight: bold;
float: right;
font-size: 22px;
line-height: 20px;
cursor: pointer;
transition: 0.3s;`;
      messageBox.style.cssText = `margin: auto; padding: 8px 10px 8px 20px; background-color: #ddffdd; border-left: 6px solid #04AA6D; position:fixed;z-index: 9999;left: 50%; top: -100px;transition: all 0.2s;`;
      messageBox.innerHTML = html;
      messageBox.prepend(cross);
      return messageBox;
    };
    let box = createBox(msg);
    document.body.appendChild(box);
    box.style.top = '10px';
    if (time) {
      setTimeout(() => {
        box.style.top = '-100px';
        setTimeout(() => box.remove(), 500);
      }, time);
    }
  }
  const makeCopyable = (el) => {
    const opacityLevel = '0.9';
    el.style.opacity = opacityLevel;
    el.style.cursor = 'pointer';
    el.addEventListener('mouseover', () => {
      el.style.opacity = '';
    });
    el.addEventListener('mouseout', () => {
      el.style.opacity = opacityLevel;
    });
    const listener = () => {
      if (navigator.clipboard) {
        navigator.clipboard.writeText(
          el.innerText
            .split('\n')
            .map((line) => line.replace(/^[\s]*[\d]+:/g, ''))
            .join('\n'),
        );
        message('Copied', 500);
      } else {
        message('Failed to copy', 500);
      }
    };
    el.addEventListener('click', listener);
  };
  const makePreCopyable = function () {
    document.querySelectorAll('pre').forEach(makeCopyable);
  };
  makePreCopyable();
  initSideMenu();
  initDarkTheme();
});
