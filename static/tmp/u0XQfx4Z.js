var allCB=[{"ident":"h2","pid":"2"},{"ident":"h3","pid":"3"},{"ident":"h4","pid":"5"},{"ident":"h5","pid":"6"},{"ident":"h6","pid":"7"},{"ident":"h7","pid":"8"}]
function onChange(e){alert(e.checked)};for(cbi in allCB)$("#"+allCB[cbi].ident).bind("change",onChange)