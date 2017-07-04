#' Query full team names for one of NBA or MLB.
#'
#' A simple wrapper to return full team names for a particular sport.
#' @param sport A string, one of 'nba' or 'mlb'
#' @keywords get, teamnames, full
#' @export
#' @examples
#' getFullTeamnames('nba')
getFullTeamnames <- function(sport) {
    if (sport == 'nba')
        c('Atlanta Hawks',          'Brooklyn Nets',          'Boston Celtics',
          'Charlotte Bobcats',      'Chicago Bulls',          'Cleveland Cavaliers',
          'Dallas Mavericks',       'Denver Nuggets',         'Detroit Pistons',
          'Golden State Warriors',  'Houston Rockets',        'Indiana Pacers',
          'Los Angeles Clippers',   'Los Angeles Lakers',     'Memphis Grizzlies',
          'Miami Heat',             'Milwaukee Bucks',        'Minnesota Timberwolves',
          'New Orleans Hornets',    'New York Knicks',        'Oklahoma City Thunder',
          'Orlando Magic',          'Philadelphia Sixers',    'Phoenix Suns',
          'Portland Trail Blazers', 'San Antonio Spurs',      'Sacramento Kings',
          'Toronto Raptors',        'Utah Jazz',              'Washington Wizards')
    else if (sport == 'mlb')
        c('arizona diamondbacks', 'atlanta braves',      'baltimore orioles',
          'boston redsox',        'chicago cubs',        'chicago whitesox',
          'cincinati reds',       'cleveland indians',   'colorado rockets',
          'detroit tigers',       'houston astros',      'kansas city royals',
          'los angeles angels',   'los angeles dodgers', 'miami marlins',
          'milwaukee brewers',    'minnesota twins',     'new york mets',
          'new york yankees',     'oakland athletics',   'philadelphia phillies',
          'pittsburgh pirates',   'san diego padres',    'seattle mariners',
          'san francisco giants', 'st louis cardinals',  'tampa bay rays',
          'texas rangers',        'toronto blue jays',   'washington nationals')
}
