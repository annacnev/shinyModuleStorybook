# js ----------------------------------------------------------------------

# Used for resizing iframes when embedding apps
resizeScript <- 
  'var dimension = [0, 0];
$(document).on("shiny:connected", function(e) {
dimension[0] = window.innerWidth;
dimension[1] = window.innerHeight;
Shiny.onInputChange("dimension", dimension);
});
$(window).resize(function(e) {
dimension[0] = window.innerWidth;
dimension[1] = window.innerHeight;
Shiny.onInputChange("dimension", dimension);
});'

linkToTabScript <- 
  "var openTab = function(tabName){
$('a', $('.sidebar')).each(function() {
if(this.getAttribute('data-value') == tabName) {
this.click()
};
});
}"