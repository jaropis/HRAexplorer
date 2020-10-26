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
    console.log(...parameters);
    fun(selector, ...parameters);
  } else {
    console.log(selector);
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

/* show and hide back card */
function show_back_card(card_id) {
  document.getElementById(card_id).style.visibility = "visible";
}

function hide_back_card(card_id) {
  console.log(card_id);
  document.getElementById(card_id).style.visibility = "hidden";
}

/* add event listeners to back-buttons so that the back card can be hidden, eg. 'btn-6-back' and "details-table-runs-flip-container"*/
function add_show_to_button(button_id, card_id) {
  var front_button = document.querySelector(button_id);
  front_button.addEventListener('click', function() {
    show_back_card(card_id);
  });
}


function add_hide_to_button(button_id, card_id) {
  var back_button = document.querySelector(button_id);
  back_button.addEventListener('click', function() {
    hide_back_card(card_id);
  });
}
