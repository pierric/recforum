var allCB=[{"ident":"h2","pid":"2"}],queue=new Array()
function onChange(p){return function(e){if(e.target.checked){queue.push(p);if(queue.length>2){var remv=queue.splice(0,queue.length-2);jQuery.each(remv,function(i,v){$("#"+v.ident).get(0).checked=false})};if(queue.length==2){$("#btnCompare").removeClass("disabled");var pids=jQuery.map(queue,function(e,i){return e.pid});$("#btnCompare").attr("href","/pcomp/"+pids.sort().join('/'))}}else{var idx=jQuery.inArray(p,queue);if(idx>-1)queue.splice(idx,1);$("#btnCompare").addClass("disabled")}}};for(cbi in allCB)$("#"+allCB[cbi].ident).bind("change",onChange(allCB[cbi]))