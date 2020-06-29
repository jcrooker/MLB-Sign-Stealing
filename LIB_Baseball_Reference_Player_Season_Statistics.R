batter_statistics_by_season_by_team<-function(team="HOU",season=2017) {
  my_url<-paste0("https://www.baseball-reference.com/teams/",team,"/",season,".shtml")
  my_con<-file(my_url,"r")
  my_txt<-readLines(my_con,-1)
  close(my_con)
  
  bat_loc<-grep("[<]caption[>]Team Batting[<][/]caption[>]",my_txt)
  ### Read Stat headers
  thead_loc<-grep("[<][/]thead[>]",my_txt)
  header_loc<-seq(bat_loc[1],thead_loc[thead_loc>bat_loc[1]][1],by=1)
  chunk<-my_txt[header_loc]
  th_loc<-grep("[<]th",chunk)
  stat<-unlist(lapply(as.list(th_loc),
                      function(i) {strsplit(strsplit(chunk[i],"data[-]stat[=]\"")[[1]][2],"\"")[[1]][1]}))
  stat<-stat[-1]
  #### End stat headers
  #### Read player stats
  end_tbody_loc<-grep("[<][/]tbody[>]",my_txt)
  end_tbody_loc<-end_tbody_loc[end_tbody_loc>bat_loc[1]][1]
  chunk<-my_txt[seq(header_loc[length(header_loc)],
                     end_tbody_loc)]
  tr_loc<-grep("[<]tr [class=\"non_qual\" ]*[>]",chunk)
  my_matrix<-matrix(NA,nrow=length(tr_loc),ncol=length(stat))
  for (i in 1:length(tr_loc)) {
    for (istat in stat) {
      isplit<-strsplit(chunk[tr_loc[i]],paste0("data-stat=\"",istat,"\""))[[1]][2]
      ielem<-trimws(strsplit(strsplit(isplit,"[>]")[[1]][2],"[<]")[[1]][1],which="both")
      my_matrix[i,which(istat==stat)]<-ielem
    }
  }
  #### End read player stats
  my_datfr<-data.frame(my_matrix,stringsAsFactors = FALSE)
  names(my_datfr)<-stat
  my_datfr$href<-rep(NA,nrow(my_datfr))
  #### Player Pos, name and age
  for (i in 1:length(tr_loc)) {
    ipos<-strsplit(strsplit(chunk[tr_loc[i]],"data-stat=\"pos\"")[[1]][2],"[<][/]strong[>]")[[1]][1]
    ipos<-strsplit(ipos,"[<]strong[>]")[[1]][2]
    ihref<-strsplit(strsplit(chunk[tr_loc[i]],"href[=]\"")[[1]][2],"\"")[[1]][1]
    iname<-strsplit(strsplit(chunk[tr_loc[i]],"[<][/]a[>]")[[1]][1],"[.]shtml\"[>]")[[1]][2]
    my_datfr$pos[i]<-ipos
    my_datfr$player[i]<-iname
    my_datfr$href[i]<-ihref
  }
  #### End player pos, name and age
  return(my_datfr)
}
batter_previous_season_statistics<-function(href="/players/m/mccanbr01.shtml",previous_season=2016) {
  my_url<-paste0("https://www.baseball-reference.com",href)
  my_con<-file(my_url,"r")
  my_txt<-readLines(my_con,-1)
  close(my_con)
  
  p_datfr<-NULL
  stdrd_loc<-grep("[<]caption[>]Standard Batting[<][/]caption[>]",my_txt)
  if (length(stdrd_loc)>0) {
    thead_end_loc<-grep("[<][/]thead[>]",my_txt)
    thead_end_loc<-thead_end_loc[thead_end_loc>stdrd_loc[1]][1]
    chunk<-my_txt[stdrd_loc[1]:thead_end_loc]
    ### Read Stats
    stat_loc<-grep("data[-]stat[=]",chunk)
    stat<-unlist(lapply(as.list(chunk[stat_loc]),
                        function(str) {strsplit(strsplit(str,"data[-]stat[=]\"")[[1]][2],"\"")[[1]][1]}))
    ### End Reat Stats
    ### Read player data
    yr_loc<-grep(paste0("[<]tr id[=]\"batting[_]standard[.]",previous_season,"\" class[=]\"full\" [>]"),my_txt)
    if (length(yr_loc)>0) {
      pdata<-vector()
      for (istat in stat) {
        istr<-my_txt[yr_loc[1]]
        istr<-gsub("[<][/]*strong[>]","",gsub("[<][/]*em[>]","",istr))
        pdata[which(istat==stat)]<-gsub("<.*?>","",strsplit(strsplit(strsplit(istr,paste0("data[-]stat[=]\"",istat,"\""))[[1]][2],"[>]")[[1]][2],"[<]")[[1]][1])
      }
      p_datfr<-data.frame(matrix(pdata,nrow=1),stringsAsFactors = FALSE)
      names(p_datfr)<-stat
      ### Identify MLB Service Years
      service_loc<-grep("[<tr id=\"batting[_]standard[.][1-2][0-9][0-9][0-9]\"",my_txt)
      v_yr<-0
      if (length(service_loc)>0) {
        service_yrs<-unlist(lapply(as.list(my_txt[service_loc]),
                                   function(str) {substr(strsplit(str,"batting[_]standard[.]")[[1]][2],1,4)})) 
        v_yr<-which(service_yrs==as.character(previous_season))
      }
      p_datfr$MLB_Years<-v_yr
      ### End Identify MLB Service Years
    }
    ### End Read Player data 
  }
  return(p_datfr)
}
team_roster_and_recent_player_stats<-function(team="HOU",base_season=2017) {
  prior<-batter_previous_season_statistics()
  p_names<-names(prior)
  prior<-cbind(prior,prior)
  names(prior)<-c(paste0("S",base_season-1,p_names),
                  paste0("S",base_season+1,p_names))
  prior[1,]<-rep(NA,ncol(prior))
  
  base_stats<-batter_statistics_by_season_by_team(team=team,season=base_season)
  other_stats<-NULL
  for (i in 1:nrow(base_stats)) {
    ilink<-base_stats$href[i]
    istats0<-batter_previous_season_statistics(href=ilink,previous_season=base_season-1) 
    Sys.sleep(runif(n=1,min=1,max=3))
    istats2<-batter_previous_season_statistics(href=ilink,previous_season=base_season+1)
    if (!is.null(istats0)) {
      names(istats0)<-names(prior)[1:length(p_names)]
    } else {
      istats0<-prior[1,1:length(p_names)]
    }
    if (!is.null(istats2)) {
      names(istats2)<-names(prior)[(length(p_names)+1):ncol(prior)]
    } else {
      istats2<-prior[1,(length(p_names)+1):ncol(prior)]
    }
    if (!is.null(other_stats)) {
      other_stats<-rbind(other_stats,cbind(istats0,istats2))
    } else {
      other_stats<-cbind(istats0,istats2)
    }
  }
  full_stats<-cbind(base_stats,other_stats)
  return(full_stats)
}
mlb_teams_by_season<-function(season=2017) {
  my_url<-c(paste0("https://www.baseball-reference.com/leagues/NL/",season,".shtml"),
            paste0("https://www.baseball-reference.com/leagues/AL/",season,".shtml"))
  my_teams<-vector()
  for (iurl in my_url) {
    my_con<-file(iurl,"r")
    my_txt<-readLines(my_con,-1)
    close(my_con)
    
    stdrd_batting<-grep("[<]caption[>]Team Standard Batting Table[<][/]caption[>]",my_txt)
    end_tbody<-grep("[<][/]tbody[>]",my_txt)
    chunk<-my_txt[stdrd_batting[1]:end_tbody[end_tbody>stdrd_batting[1]][1]]
    team_loc<-grep(paste0("href[=]\"[/]teams[/][A-Z]*[/]",season,"[.]shtml"),chunk)
    iteam<-unlist(lapply(as.list(chunk[team_loc]),
                         function(str) {strsplit(strsplit(str,"[/]teams[/]")[[1]][2],"[/]")[[1]][1]}))
    my_teams<-c(my_teams,iteam)
  }
  return(my_teams)
}
archive_team_rooster_season_performance<-function(season=2017) {
  my_teams<-mlb_teams_by_season(season=season)
  for (iteam in my_teams) {
    ifile<-paste0("C:/research/mlb/",season,"/",iteam,"_",season,".csv")
    if (!file.exists(ifile)) {
      Sys.sleep(runif(n=1,min=1,max=3))
      idat<-team_roster_and_recent_player_stats(team=iteam,base_season=season)
      write.table(idat,file=ifile,sep=",",row.names=FALSE)
    }
  }
}
