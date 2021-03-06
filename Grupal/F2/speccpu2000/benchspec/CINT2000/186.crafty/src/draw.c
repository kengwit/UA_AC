#include <stdio.h>
#include "chess.h"
#include "data.h"

/* last modified 09/19/96 */
/*
********************************************************************************
*                                                                              *
*   DrawScore() is used to determine how good or bad a draw is in the current  *
*   game situation.  It depends on several factors, including the stage of the *
*   game (opening [draw=bad], middlegame [draw=somewhat bad] and endgames      *
*   [draw=default_draw_score.])  Additionally, it looks at the opponent's time *
*   remaining in the game, and if it's low, it will eschew the draw and press  *
*   the opponent to win before his flag falls.                                 *
*                                                                              *
********************************************************************************
*/
int DrawScore(void)
{
  register int draw_score;
/*
 ----------------------------------------------------------
|                                                          |
|   if playing a computer, return the default draw score   |
|   regardless of the phase of the game or time left.      |
|                                                          |
 ----------------------------------------------------------
*/
  if (draw_score_is_zero) return(default_draw_score);
/*
 ----------------------------------------------------------
|                                                          |
|   first, set the draw score based on the phase of the    |
|   game, as would be done normally, anyway.               |
|                                                          |
 ----------------------------------------------------------
*/
  if (opening) 
    draw_score=default_draw_score-66;
  else if (middle_game)
    draw_score=default_draw_score-33;
  else
    draw_score=default_draw_score;
/*
 ----------------------------------------------------------
|                                                          |
|   now that the base draw score has been set, look at the |
|   opponent's remaining time and set the draw score to a  |
|   lower value if his time is low.                        |
|                                                          |
|   the first test:  if both have less than one minute on  |
|   the clock, then if crafty has 2x the opponent's time   |
|   the draw should be avoided if possible.                |
|                                                          |
 ----------------------------------------------------------
*/
  if (tc_increment <= 200) {
    if (tc_time_remaining_opponent < 6000)
      draw_score=default_draw_score-50;
    if (tc_time_remaining_opponent < 3000)
      draw_score=default_draw_score-100;
  }
  return(draw_score);
}
