$(document).ready(function(){

  // data based rendering
  $("#title").html(data[0].display + ' &ndash; ' + data[0].tnice);
  if (data[0].Keep == true) {
    $("#tag-si").addClass("is-success");
    var SI = Math.round(10*data[0].SI_Est)/10;
    if (data[0].SI2_Est > 100) {
      var arrow = '<span class="icon"><i class="fa fa-arrow-up"></i></span>'
    } else {
      var arrow = '<span class="icon"><i class="fa fa-arrow-down"></i></span>'
    }
    $("#tag-si").html('<span>' + SI + '%</span> ' + arrow);
    if (SI > 90) {
      $("#tag-si").addClass("is-success");
    } else {
      if (SI > 80) {
        $("#tag-si").addClass("is-warning");
      } else {
        $("#tag-si").addClass("is-danger");
      }
    }
  } else {
    $("#SI").hide();
    $("#results").hide();
  };
  if (data[0].model_north == true) {
    $("#tag-north").addClass("is-success");
    $("#tag-north").text("Yes");
  } else {
    $("#tag-north").addClass("is-danger");
    $("#tag-north").text("No");
  };
  if (data[0].model_south == true) {
    $("#tag-south").addClass("is-success");
    $("#tag-south").text("Yes");
  } else {
    $("#tag-south").addClass("is-danger");
    $("#tag-south").text("No");
  };
  $("#tag-version").text(data[0].version);
  $("#tag-hf").text(data[0].hf);
  $("#tag-veg").text(data[0].veg);
  if (data[0].Comments == "") {
    $("#comments").hide();
  } else {
    $("#message").text(data[0].Comments);
  }

  $("#sppweb").attr("href", "http://species.abmi.ca/pages/species/" + data[0].Taxon + '/' + data[0].SpeciesID + '.html');

});
