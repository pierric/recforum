function cancel(){$.getJSON("http://192.168.56.101:3000/punlock/7",null,function(e){window.location.assign("http://192.168.56.101:3000/aview/1")})};$("#btnParagraphSave").click(function(){$("form").submit()});$("#btnParagraphModCancel").click(cancel);$("#btnParagraphCancel").click(cancel);$(window).unload(cancel)