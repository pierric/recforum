var articlePvt = $("div.required:has(input#articlePvt)");
articlePvt.css("float","left");
articlePvt.css("margin", "2px 2em 0 2em");
var articleDft = $("div.required:has(input#articleDft)");
articleDft.css("float","left");
articleDft.css("margin", "2px 2em 0 0");
$("#articleMod").hide();
$("#btnArticleModCancel").click(function(){
    $("#articleMod").hide();
});
$("#btnArticleMod").click(function(){
    $("#articleMod").show();
});
$("#paragraphMovSubmit").hide();
$("#btnParagraphMov").click(function(){
    $("#paragraphMovSubmit").show();
});

$("#movPanel").hide();
$("div.required:has(input#paragraphId)").hide();
$(".btnParagraphMov").click(function (e){
    alert($(e.target).parents(.paragraph).length);
});
