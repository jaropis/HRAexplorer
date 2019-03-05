function click_more(buttonID) {
  moreButtonId = document.getElementById('btn-1');
  text = buttonID.id.split("_")[2];
  Shiny.setInputValue("foo", text);
  moreButtonId.click();
  console.log(buttonID);
}
