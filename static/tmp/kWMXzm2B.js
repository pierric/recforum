
(function(){new nicEditor({fullPanel:true}).panelInstance("topicDsc")})();
var topicPvt = $("div.required:has(input#topicPvt)");
topicPvt.css("float","left");
topicPvt.css("margin", "2px 2em 0 2em");
var topicDft = $("div.required:has(input#topicDft)");
topicDft.css("float","left");
topicDft.css("margin", "2px 2em 0 0");
$("#btnTopicMod").click(function(){
    alert("OK.");
}
