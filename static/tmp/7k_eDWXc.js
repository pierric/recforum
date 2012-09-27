function cancel() {
    $.getJSON(http://192.168.56.101:3000/punlock/6,null,
        function(e) {
            alert(e);
        });
}
$("#btnParagraphModCancel").click(cancel);

