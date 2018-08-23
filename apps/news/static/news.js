function byId(id) {
  return document.getElementById(id);
}

function vote(node) {
  var v = node.id.split(/_/);   // {'up', '123'}
  var item = v[1];

  // adjust score
  try{
    var score = byId('score_' + item);
    var newscore = parseInt(score.innerHTML) + (v[0] == 'up' ? 1 : -1);
    score.innerHTML = newscore + (newscore == 1 ? ' point' : ' points');
  } catch(err) {} //ignore

  // hide arrows
  byId('up_'   + item).style.visibility = 'hidden';
  try { byId('down_' + item).style.visibility = 'hidden'; }
  catch(err) {} // ignore

  // ping server
  var ping = new Image();
  ping.src = node.href;

  return false; // cancel browser nav
}
