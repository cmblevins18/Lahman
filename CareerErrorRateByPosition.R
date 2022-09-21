#Run this after ErrorRateByPosition

summstats <- function(d) {
  require('plyr')
  NAmassage <- function(x) {
    # Takes a column vector and replaces NAs by zeros
    x[is.na(x)] <- 0
    x
  }
  vars <- c('G', 'PO', 'A', 'E', 'rfact',
            'rfacta', 'Chance', 'E.rate', 'E.res')
  d2 <- apply(d[, vars], 2, NAmassage)
  d2 <- if(is.vector(d2)) {as.data.frame(as.list(d2)) } else {
    as.data.frame(d2) }
  d2 <- mutate(d2,
               rf=rfact/G,
               rfa=rfacta/G,
               AChn=Chance/G,
               E.rate.G=E.rate/G,
               E.res.G=E.res/G,
  )
  data.frame(d, d2[, 12:16])
}


careerTotals <- function(d) {
  require('plyr')
  sumstats <-
    as.data.frame(as.list(colSums(as.matrix(d[, 6:24, drop = FALSE]),
                                  na.rm = TRUE)))
  cstats <- with(d, data.frame(
    beginYear = min(yearID),
    endYear = max(yearID),
    nyears = sum(stint == 1L),
    nteams = length(unique(teamID))  ))
  extrastats <- mutate(sumstats,
                       BA = ifelse(AB > 0, round(H/AB, 3), NA),                   # batting average
                       PA = AB + BB + HBP + SH + SF,                              # plate appearances
                       TB = H + X2B + 2 * X3B + 3 * HR,                           # total bases
                       SlugplayerBattingProfiles <- dlply(Batting, .(playerID), summstats)Pct = ifelse(AB > 0, round(TB/AB, 3), NA),             # slugging percentage
                       OBP = ifelse(PA > 0,                                       # on-base percentage
                                    round((H + BB + HBP)/(PA - SH - SF), 3), NA),
                       OPS = round(OBP + SlugPct, 3),
                       BABIP = ifelse(AB > 0, round(H/(AB - SO), 3), NA)  )
  cbind(cstats, extrastats)
}



playerfieldingProfiles <- dlply(rfOFres, .(playerID),summstats)