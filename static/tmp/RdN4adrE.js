var allCB=[{"ident":"h2","pid":"2"},{"ident":"h3","pid":"3"},{"ident":"h4","pid":"5"},{"ident":"h5","pid":"6"},{"ident":"h6","pid":"7"},{"ident":"h7","pid":"8"}],queue=new Array()
function onChange(p){return function(e){if(e.target.checked){queue.push(p);if(queue.length>2)queue.splice(0,queue.length-2)}else{var idx=jQuery.inArray(p,queue);if(idx>-1)queue.splice(idx,1)};alert(queue)}};for(cbi in allCB)$("#"+allCB[cbi].ident).bind("change",onChange(allCB[cbi].pid))