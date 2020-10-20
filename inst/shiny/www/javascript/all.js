// this list is 'experimental' and comes from the tabs ids in the UI
var tabs_list = {
  "staticpp": '1',
  "dynamicpp": '5',
  "staticruns": "2",
  "dynamicruns": "6",
  "dynamicspectral": "7",
  "dynamicquality": "8"};

function click_more(buttonID) {
  clicked_line = buttonID.id.split("_")[3];
  input_type = buttonID.id.split("_")[2];
  Shiny.setInputValue(input_type, clicked_line);
  moreButtonId = document.getElementById('btn-' + tabs_list[input_type] + '-front');
  moreButtonId.click();
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
