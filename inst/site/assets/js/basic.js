$(document).ready(function(){

  $("#footer").html('&copy; <a href="http://www.abmi.ca/">Alberta Biodiversity Monitoring Institute</a> (2014&ndash;' + (new Date()).getFullYear() + ') under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">CC BY-SA 4.0</a> license.');

  // launch modal
  $(".image-modal").click(function(){
    var target = $(this).data("target");
    $(target).addClass("is-active");
  });
  // close modal by clicking (X)
  $(".modal-close").click(function() {
    $(".modal").removeClass("is-active");
  });
  // close modal by Esc
  $(document).keyup(function(e) {
    if (e.keyCode == 27) {
      $(".modal").removeClass("is-active");
    }
  });

  // burger navbar
  var burger = document.querySelector('.burger');
  var menu = document.querySelector('#'+burger.dataset.target);
  burger.addEventListener('click', function() {
    burger.classList.toggle('is-active');
    menu.classList.toggle('is-active');
  });

});
