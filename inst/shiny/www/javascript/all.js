function click_more(buttonID) {
  moreButtonId = document.getElementById('btn-1-front');
  text = buttonID.id.split("_")[3];
  Shiny.setInputValue("foo", text);
  moreButtonId.click();
  console.log(buttonID);
}

function waitForEl(selector, fun, parameters) {
  // selector = 'div.wrapper', parameters = [timeout, sleep, interval]
  if(document.querySelector(selector)) {
    fun(selector, ...parameters);
  } else {
    console.log("no div.wrapper yet");
    setTimeout(function() {
      waitForEl(selector, fun, parameters);
    }, 500);
  }
}

// Function starting spinner
function startSpinner(timeout, sleep, interval) {
    var element = document.querySelector('div.wrapper');
    element.setAttribute('style', 'overflow: hidden');
    setInterval(function() {
      var spinner = $('div.spinnerLoading.overlay');
      if ($('html').hasClass('shiny-busy')) {
        setTimeout(function () {
          if ($('html').hasClass('shiny-busy')) {
          spinner.show();
        }}, timeout);
      } else {
        if (spinner.is(':visible')) {
          setTimeout(function() {
            if (!($('html').hasClass('shiny-busy'))) {
              spinner.hide();
            }
          }, sleep);
        }
      }
    }, interval);
}
