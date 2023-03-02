AddHtmlStyle <-
function (style, x, y, backgroundColor = "", color = "", fontWeight = "") 
{
 X=Y=NULL
    if (is.null(style)) {
        style = as.data.frame(cbind(x, y, backgroundColor, color, 
            fontWeight))
        colnames(style) = c("X", "Y", "backgroundColor", "color", 
            "fontWeight")
    }
    else { 
        if (nrow(subset(style, X == x & Y == y)) > 0) { 
            style[style[,"X"] == x & style[,"Y"] == y, "backgroundColor"] = backgroundColor
            style[ style[,"X"] == x & style[,"Y"] == y,"color"] = color
            style[ style[,"X"] == x & style[,"Y"] == y,"fontWeight"] = fontWeight
        }
        else {
            newStyle = cbind(x, y, backgroundColor, color, fontWeight)
            colnames(newStyle) = c("X", "Y", "backgroundColor", 
                "color", "fontWeight")
            style = rbind(style, newStyle)
        }
    }
    return(style)
}
