#calculate overlap
catchments.radolan.overlap <- ZonalOverlap.getWeightedOverlap(xtruso.radolan.sample, xtruso.catchments, polygons.id="GKZ")

#store data
save(catchments.radolan.overlap, file = "data-xtruso/catchments-radolan.Rdata")
