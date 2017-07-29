myplotMapDomFlows <- function (mat,
                               spdf,
                               spdfid,
                               w,
                               wid,
                               wvar,
                               wcex = 0.05,
                               inoutflow,
                               legend.flows.pos = "topright",
                               legend.flows.title = "flow intensity",
                               legend.nodes.pos = "topleft",
                               legend.node.txt = c("Dominant",
                                                   "Intermediary",
                                                   "Dominated",
                                                   "Size proportional\nto sum of inflows"),
                               add = FALSE)
{
    pts <- data.frame(sp::coordinates(spdf), id = spdf@data[,
                                                            spdfid])
    names(pts)[1:2] <- c("long", "lat")
    w <- w[, c(wid, wvar)]
    names(w) <- c("id", "var")
    pts <- merge(pts,
                 w,
                 by.x = "id",
                 by.y = "id",
                 all.x = T)
    bbbox <- sp::bbox(spdf)
    x1 <- bbbox[1]
    y1 <- bbbox[2]
    x2 <- bbbox[3]
    y2 <- bbbox[4]
    sfdc <- (x2 - x1) * (y2 - y1)
    sc <- sum(pts$var, na.rm = TRUE)
    pts$cex <- sqrt((pts$var * wcex * sfdc / sc) / pi)
    pts <- pts[order(pts$cex, decreasing = TRUE), ]
    pts <- pts[pts$cex > 0, ]
    colnames(mat) <- paste("X", colnames(mat), sep = "")
    row.names(mat) <- paste("X", row.names(mat), sep = "")
    fdom <- reshape2::melt(mat)
    names(fdom) <- c("i", "j", "fij")
    fdom <- fdom[fdom$fij > 0, ]
    fdom$i <- substr(x = fdom$i, 2, nchar(as.character(fdom$i)))
    fdom$j <- substr(x = fdom$j, 2, nchar(as.character(fdom$j)))
    fdom <- merge(
        fdom,
        pts,
        by.x = "i",
        by.y = "id",
        all.x = T,
        suffixes = c("i", "j")
    )
    fdom <- merge(
        fdom,
        pts,
        by.x = "j",
        by.y = "id",
        all.x = T,
        suffixes = c("i", "j")
    )
    fdom$width <- (fdom$fij * 8 / (max(fdom$fij) - min(fdom$fij))) +
        2
    pts$col <- "green"
    # barplot(rep(1,times=500),col=rainbow(1500)[1:500],border=rainbow(1500)[1:500],axes=FALSE)
    for(i in 1:nrow(pts)){
        color_index <- (inoutflow[FID==as.character(pts[i,"id"]), rate] - 0.35) / (0.65 - 0.35)
        if(color_index < 0.35) {color_index <- 0.35} else if (color_index > 0.65) {color_index <- 0.65}
        pts[i,"col"] <- rainbow(300)[1:100][round(color_index * 100)]
    }
    # pts[pts$id %in% fdom$j & !pts$id %in% fdom$i, "col"] <- "red"
    # pts[pts$id %in% fdom$j & pts$id %in% fdom$i, "col"] <- "orange"
    # pts[!pts$id %in% fdom$j & pts$id %in% fdom$i, "col"] <- "yellow"
    # pts <- pts[pts$col != "green", ]
    if (add == FALSE) {
        sp::plot(spdf,
                 col = NA,
                 border = NA,
                 add = F)
    }
    segments(
        fdom$longi,
        fdom$lati,
        fdom$longj,
        fdom$latj,
        col = "#00000020",
        lwd = fdom$width
    )
    symbols(
        pts[, c("long", "lat")],
        circles = pts$cex,
        add = TRUE,
        bg = pts$col,
        fg = "white",
        inches = F
    )
    text(
        pts[, c("long", "lat")],
        labels = pts$id,
        col = "black",
        cex = sqrt(pts$cex * 1000)
    )
    segments(
        fdom$longi,
        fdom$lati,
        fdom$longj,
        fdom$latj,
        col = "#00000003",
        lwd = fdom$width
    )
    LegendPropLines(
        pos = legend.flows.pos,
        legTitle = legend.flows.title,
        legTitleCex = 0.8,
        legValuesCex = 0.6,
        varvect = c(min(fdom$fij),
                    max(fdom$fij)),
        sizevect = c(2, 10),
        col = "black",
        frame = FALSE,
        round = 0
    )
    legend(
        x = legend.nodes.pos,
        legend = legend.node.txt,
        cex = c(0.8),
        pt.cex = c(2.8, 2, 1, 0),
        bty = "n",
        pt.bg = c("red",
                  "orange", "yellow", NA),
        pch = c(21, 21, 21, 21)
    )
}


LegendPropLines <-
    function(pos = "topleft",
             legTitle = "Title of the legend",
             legTitleCex = 0.8,
             legValuesCex = 0.6,
             varvect,
             sizevect,
             col = "red",
             frame = FALSE,
             round = 0) {
        positions <- c(
            "bottomleft",
            "topleft",
            "topright",
            "bottomright",
            "left",
            "right",
            "top",
            "bottom",
            "middle"
        )
        if (pos %in% positions) {
            # extent
            x1 <- par()$usr[1]
            x2 <- par()$usr[2]
            y1 <- par()$usr[3]
            y2 <- par()$usr[4]
            xextent <- x2 - x1
            yextent <- y2 - y1
            
            # variables internes
            paramsize1 <- 25
            paramsize2 <- 40
            width <- (x2 - x1) / paramsize1
            height <- width / 1.5
            delta1 <-
                min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
            delta2 <-
                (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2)) / 2 # Petit eccart entre les objets
            
            
            rValmax <- max(varvect, na.rm = TRUE)
            rValmin <- min(varvect, na.rm = TRUE)
            rValextent <- rValmax - rValmin
            rLegmax <- max(sizevect, na.rm = TRUE)
            rLegmin <- min(sizevect, na.rm = TRUE)
            rLegextent <- rLegmax - rLegmin
            
            rVal <-
                c(rValmax,
                  rValmax - rValextent / 3 ,
                  rValmax - 2 * (rValextent / 3),
                  rValmin)
            rLeg <-
                c(rLegmax,
                  rLegmax - rLegextent / 3 ,
                  rLegmax - 2 * (rLegextent / 3),
                  rLegmin)
            rVal <- round(rVal, round)
            
            # xsize & ysize
            
            longVal <-
                rVal[strwidth(rVal, cex = legValuesCex) == max(strwidth(rVal, cex = legValuesCex))][1]
            #if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- paste(">=",breakval)}}
            legend_xsize <-
                max(
                    width + strwidth(longVal, cex = legValuesCex) - delta2,
                    strwidth(legTitle, cex = legTitleCex) - delta1
                )
            
            legend_ysize <-
                8 * delta1 + strheight(legTitle, cex = legTitleCex)
            
            # Position
            if (pos == "bottomleft") {
                xref <- x1 + delta1
                yref <- y1 + delta1
            }
            if (pos == "topleft") {
                xref <- x1 + delta1
                yref <- y2 - 2 * delta1 - legend_ysize
            }
            if (pos == "topright") {
                xref <-
                    x2 - 2 * delta1 - legend_xsize
                yref <- y2 - 2 * delta1 - legend_ysize
            }
            if (pos == "bottomright") {
                xref <- x2 - 2 * delta1 - legend_xsize
                yref <- y1 + delta1
            }
            if (pos == "left") {
                xref <- x1 + delta1
                yref <- (y1 + y2) / 2 - legend_ysize / 2 - delta2
            }
            if (pos == "right") {
                xref <-
                    x2 - 2 * delta1 - legend_xsize
                yref <- (y1 + y2) / 2 - legend_ysize / 2 - delta2
            }
            if (pos == "top") {
                xref <-
                    (x1 + x2) / 2 - legend_xsize / 2
                yref <- y2 - 2 * delta1 - legend_ysize
            }
            if (pos == "bottom") {
                xref <- (x1 + x2) / 2 - legend_xsize / 2
                yref <- y1 + delta1
            }
            if (pos == "middle") {
                xref <-
                    (x1 + x2) / 2 - legend_xsize / 2
                yref <- (y1 + y2) / 2 - legend_ysize / 2 - delta2
            }
            
            
            # Frame
            if (frame == TRUE) {
                rect(
                    xref - delta1,
                    yref - delta1,
                    xref + legend_xsize + delta1 * 2,
                    yref + legend_ysize + delta1 * 2,
                    border = "black",
                    col = "white"
                )
            }
            
            mycol <- col
            
            jump <- delta1
            for (i in 4:1) {
                if (rLeg[i] < 0.2) {
                    rLeg[i] <-
                        0.2
                } # TAILLE DES LIGNE MINIMALES (A METTRE AUSSI SUR LES CARTES)
                
                segments(
                    xref,
                    yref + jump,
                    xref + width,
                    yref + jump,
                    col = mycol,
                    lwd = rLeg[i],
                    lend = 1
                )
                text(
                    xref + width + delta2 ,
                    y = yref + jump,
                    rVal[i],
                    adj = c(0, 0.5),
                    cex = legValuesCex
                )
                jump <- jump + 2 * delta1 # ICI AMELIORER
            }
            text(
                x = xref ,
                y = yref + 9 * delta1,
                legTitle,
                adj = c(0, 0),
                cex = legTitleCex
            )
        }
    }

