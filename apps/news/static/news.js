
window.onload = function() {
  var votelinks = byClass(document, "votelink");
  for (var i = votelinks.length - 1; i >= 0; i--) {
    votelinks[i].addEventListener('click', function (e) {
      e.preventDefault();
      vote(this);
    }, false);
  }
}

function byId(e, id) {
  return e.getElementById(id);
}

function byClass(e, c) {
    return e.querySelectorAll("." + c);
}

function vote(node) {

 //adjust score
  var id = node.getAttribute("data-id");
  var dir = node.getAttribute("data-dir");
  var elem = byId(document, 'score_' + id);
 
  if(elem !== null) {

    var score = parseInt(elem.getAttribute("data-score"));
    var newscore = (dir == "up")? score + 1: score - 1;

    elem.innerHTML = newscore + ((newscore == 1)? ' point' : ' points');
  }
  
  // hide arrows
  node.parentNode.style.visibility = "hidden";

  //ping server
  var ping = new Image();
  ping.src = node.href;
}
