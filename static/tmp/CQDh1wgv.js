(function(){new nicEditor({fullPanel:true}).panelInstance("articleDsc")})();var articlePvt=$("div.required:has(input#articlePvt)");articlePvt.css("float","left");articlePvt.css("margin","2px 2em 0 2em");var articleDft=$("div.required:has(input#articleDft)");articleDft.css("float","left");articleDft.css("margin","2px 2em 0 0");$("#btnArticleModCancel").click(function(){$("#articleMod").hide()});$("#btnArticleMod").click(function(){$("#articleMod").toggle()});$("#btnParagraphMov").click(function(){$("#paragraphMovSubmit").toggle()});$("div.required:has(input#paragraphId)").hide();$(".btnParagraphMov").click(function(e){var anchor=$(e.target),pardiv=anchor.parents(".paragraph"),mp=$("#movPanel");mp.hide(100);mp.detach();pardiv.after(mp);mp.show(400);$("#paragraphId").attr("value",anchor.attr("paragraph"));$("moveOffset").attr("value",0)});$("#btnArticleMovCancel").click(function(){$("#movPanel").hide(100)})