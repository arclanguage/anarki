
window.onload = function() {
    var votelinks = byClass(document, "votelink");
    for (var i = votelinks.length - 1; i >= 0; i--) {
      votelinks[i].addEventListener('click', function (e) {
        e.preventDefault();
        vote(this);
      }, false);
    };    
  }
}

function byId(e, id) {
  return e.getElementById(id);
}

function byClass(e, c) {
  if(e.getElementsByClassName) {
    return e.getElementsByClassName(c);
  }
  else if (e.querySelectorAll) {
    return e.querySelectorAll("." + c);
  }
  else {
    var elems = [];
    var search = e.getElementsByTagName("*");
    var regex = new RegExp("(^|\\s)" + c + "(\\s|$)");

    for (var i = search.length - 1; i >= 0; i--) {
      if(regex.test(search[i].className)) {
        elems.push(search[i]);
      }
    }

    return elems;
  }
}

function vote(node) {
 //adjust score

  var id = node.getAttribute("data-id");
  var score_elem = byId(document, 'score_' + id);

  if(score_elem !== null) {
    var dir = node.getAttribute("data-dir");
    var score = parseInt(node.getAttribute("data-score"));
    var newscore = (dir == "up")? score + 1: score - 1;

    score_elem.innerHTML = newscore + (newscore == 1)? ' point' : ' points';
  }
  
  // hide arrows
  node.style.visibility = "hidden";

  //ping server
  var ping = new Image();
  ping.src = node.href;

}
