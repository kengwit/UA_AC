#if !defined(DATA_INCLUDED)

#  define DATA_INCLUDED

extern   char           version[6];

extern   PLAYING_MODE   mode;
extern   int            batch_mode;
extern   int            crafty_rating;
extern   int            opponent_rating;
extern   int            number_auto_kibitzers;
extern   int            number_of_computers;
extern   int            number_of_GMs;
extern   int            number_of_IMs;
extern   int            time_used;
extern   int            time_used_opponent;
extern   int            auto_kibitzing;
extern   int            total_moves;
extern   int            initialized;
extern   int            early_exit;
extern   int            new_game;
extern   char           auto_kibitz_list[100][20];
extern   char           GM_list[100][20];
extern   char           IM_list[100][20];
extern   char           computer_list[100][20];
extern   FILE           *input_stream;
extern   FILE           *book_file;
extern   FILE           *books_file;
extern   FILE           *history_file;
extern   FILE           *log_file;
extern   FILE           *auto_file;
extern   FILE           *book_lrn_file;
extern   FILE           *position_file;
extern   FILE           *position_lrn_file;
extern   int            log_id;
extern   int            output_format;
extern   int            EGTBlimit;
extern   char           whisper_text[500];
extern   int            whisper_value;
extern   int            whisper_depth;
extern   int            last_mate_score;
extern   int            last_opponent_move;

extern   char           pgn_event[32];
extern   char           pgn_date[32];
extern   char           pgn_round[32];
extern   char           pgn_site[32];
extern   char           pgn_white[64];
extern   char           pgn_white_elo[32];
extern   char           pgn_black[64];
extern   char           pgn_black_elo[32];
extern   char           pgn_result[32];
extern   char           log_filename[64];
extern   char           history_filename[64];

extern   int            number_of_solutions;
extern   int            solutions[10];
extern   int            solution_type;
extern   int            default_draw_score;
extern   int            over;
extern   int            ics;
extern   int            auto232;
extern   int            auto232_delay;
extern   int            xboard;
extern   int            whisper;
extern   int            channel;
extern   char           channel_title[32];
extern   char           book_path[128];
extern   char           log_path[128];
extern   char           tb_path[128];
extern   char           cmd_buffer[512];
extern   char           *args[32];
extern   char           buffer[512];
extern   int            nargs;
extern   int            kibitz;
extern   int            move_number;
extern   int            wtm;
extern   int            crafty_is_white;
extern   int            iteration_depth;
extern   int            last_search_value;
extern   int            previous_search_value;
extern   int            search_failed_high;
extern   int            search_failed_low;
extern   int            largest_positional_score;
extern   int            root_alpha;
extern   int            root_beta;
extern   int            root_value;
extern   int            root_wtm;
extern   int            root_move;
extern   int            root_total_white_pieces;
extern   int            root_total_white_pawns;
extern   int            root_total_black_pieces;
extern   int            root_total_black_pawns;
extern   int            nodes_per_second;
extern   int            cpu_percent;

extern   int            tb_probes;
extern   int            tb_probes_successful;

extern   int            opening;
extern   int            middle_game;
extern   int            end_game;
extern   int            analyze_mode;
extern   int            annotate_mode;
extern   int            test_mode;
extern   int            analyze_move_read;
extern   signed char    resign;
extern   signed char    resign_counter;
extern   signed char    resign_count;
extern   signed char    draw_counter;
extern   signed char    draw_count;
extern   char           audible_alarm;
extern   char           hint[16];
extern   int            post;
extern   int            search_depth;
extern   int            search_move;
extern   int            easy_move;
extern   TIME_TYPE      time_type;
extern   int            time_limit;
extern   int            absolute_time_limit;
extern   int            search_time_limit;
extern   int            nodes_between_time_checks;
extern   int            next_time_check;

extern   int            time_abort;
extern   signed char    pondering;   /* program is thinking on opponent's time */
extern   signed char    thinking;    /* program is searching on its time       */
extern   signed char    puzzling;    /* program is puzzling about a move to ponder */
extern   signed char    booking;     /* program is searching, following book moves */
extern   signed char    abort_search;
extern   int            ponder;
extern   int            ponder_move;
extern   int            made_predicted_move;
extern   int            force;
extern   int            ponder_moves[220];
extern   int            num_ponder_moves;
extern   char           initial_position[80];

extern   unsigned int   opponent_start_time, opponent_end_time;
extern   unsigned int   program_start_time, program_end_time;
extern   unsigned int   start_time, end_time;
extern   unsigned int   elapsed_start, elapsed_end;
extern   unsigned int   nodes_searched;
extern   unsigned int   evaluations;
extern   int            predicted;
extern   signed char    transposition_id;
extern   int            transposition_probes;
extern   int            transposition_hits;
extern   int            pawn_probes;
extern   int            pawn_hits;
extern   int            check_extensions_done;
extern   int            recapture_extensions_done;
extern   int            passed_pawn_extensions_done;
extern   int            one_reply_extensions_done;

extern   int            ansi;
extern   int            trace_level;
extern   int            verbosity_level;
extern   int            burp;
extern   unsigned int   noise_level;

extern   int            book_move;
extern   int            moves_out_of_book;
extern   int            book_accept_mask;
extern   int            book_reject_mask;
extern   int            book_random;
extern   int            book_search_trigger;
extern   int            book_selection_width;
extern   int            show_book;
extern   int            learning;

extern   int            tc_moves;
extern   int            tc_time;
extern   int            tc_time_remaining;
extern   int            tc_time_remaining_opponent;
extern   int            tc_moves_remaining;
extern   int            tc_secondary_moves;
extern   int            tc_secondary_time;
extern   int            tc_increment;
extern   int            tc_sudden_death;
extern   int            tc_operator_time;
extern   int            tc_safety_margin;
extern   int            draw_score_is_zero;
extern   int            usage_level;

extern   int            log_hash;
extern   int            log_pawn_hash;
extern   int            hash_table_size;
extern   int            pawn_hash_table_size;

extern   int            hash_maska;
extern   int            hash_maskb;
extern   unsigned int   pawn_hash_mask;
extern   HASH_ENTRY      *trans_ref_wa;
extern   HASH_ENTRY      *trans_ref_wb;
extern   HASH_ENTRY      *trans_ref_ba;
extern   HASH_ENTRY      *trans_ref_bb;
extern   PAWN_HASH_ENTRY *pawn_hash_table;

extern   int            hash_move[MAXPLY];
extern   int            history_w[4096], history_b[4096];
extern   int            killer_move1[MAXPLY];
extern   int            killer_move2[MAXPLY];
extern   int            killer_count1[MAXPLY];
extern   int            killer_count2[MAXPLY];
extern   BITBOARD       replist_w[50+MAXPLY/2];
extern   BITBOARD       replist_b[50+MAXPLY/2];
extern   BITBOARD       *rephead_w;
extern   BITBOARD       *rephead_b;

extern   int            unblocked_pawns[9];
extern   int            p_values[15];
extern   int            current_move[MAXPLY];
extern   int            *last[MAXPLY];
extern   signed char    in_check[MAXPLY];
extern   signed char    extended_reason[MAXPLY];
extern   signed char    current_phase[MAXPLY];
extern   int            move_list[5120];
extern   int            sort_value[256];
extern   signed char    searched_this_root_move[256];
extern   unsigned int   root_nodes[256];
extern   CHESS_PATH     pv[MAXPLY];
extern   CHESS_PATH     last_pv;
extern   int            last_value;
extern   NEXT_MOVE      next_status[MAXPLY];
extern   SEARCH_POSITION position[MAXPLY+2];
extern   BITBOARD       save_hash_key[MAXPLY+2];
extern   unsigned int   save_pawn_hash_key[MAXPLY+2];

extern   char           white_outpost[64];
extern   char           black_outpost[64];
extern   char           square_color[64];
extern   int            passed_pawn_value[8];
extern   int            supported_passer[8];
extern   int            reduced_material_passer[20];
extern   int            pawn_advance[8];
extern   int            outside_passed[128];
extern   int            pawn_value_w[64];
extern   int            pawn_value_b[64];
extern   int            knight_value_w[64];
extern   int            knight_value_b[64];
extern   int            bishop_value_w[64];
extern   int            bishop_value_b[64];
extern   int            rook_value_w[64];
extern   int            rook_value_b[64];
extern   int            queen_value_w[64];
extern   int            queen_value_b[64];
extern   int            king_value_w[64];
extern   int            king_value_b[64];
extern   char           king_defects_w[64];
extern   char           king_defects_b[64];

extern   int            b_n_mate_dark_squares[64];
extern   int            b_n_mate_light_squares[64];
extern   int            mate[64];

extern   char           push_extensions[64];

extern   signed char    directions[64][64];
extern   BITBOARD       w_pawn_attacks[64];
extern   BITBOARD       b_pawn_attacks[64];
extern   BITBOARD       knight_attacks[64];
extern   BITBOARD       bishop_attacks[64];
extern   BITBOARD       bishop_attacks_rl45[64][256];
extern   BITBOARD       bishop_attacks_rr45[64][256];
extern   int            bishop_mobility_rl45[64][256];
extern   int            bishop_mobility_rr45[64][256];
extern   int            bishop_shift_rl45[64];
extern   int            bishop_shift_rr45[64];
extern   BITBOARD       rook_attacks[64];
extern   BITBOARD       rook_attacks_r0[64][256];
extern   BITBOARD       rook_attacks_rl90[64][256];
extern   int            rook_mobility_r0[64][256];
extern   int            rook_mobility_rl90[64][256];

extern   CHESS_POSITION search;
extern   CHESS_POSITION display;

extern   BITBOARD       queen_attacks[64];
extern   BITBOARD       king_attacks[64];
extern   BITBOARD       king_attacks_1[64];
extern   BITBOARD       king_attacks_2[64];
extern   BITBOARD       obstructed[64][64];

extern   unsigned int   w_pawn_random32[64];
extern   unsigned int   b_pawn_random32[64];
extern   BITBOARD       w_pawn_random[64];
extern   BITBOARD       b_pawn_random[64];
extern   BITBOARD       w_knight_random[64];
extern   BITBOARD       b_knight_random[64];
extern   BITBOARD       w_bishop_random[64];
extern   BITBOARD       b_bishop_random[64];
extern   BITBOARD       w_rook_random[64];
extern   BITBOARD       b_rook_random[64];
extern   BITBOARD       w_queen_random[64];
extern   BITBOARD       b_queen_random[64];
extern   BITBOARD       w_king_random[64];
extern   BITBOARD       b_king_random[64];
extern   BITBOARD       enpassant_random[65];
extern   BITBOARD       castle_random_w[2];
extern   BITBOARD       castle_random_b[2];
extern   BITBOARD       wtm_random[2];
extern   BITBOARD       endgame_random_w;
extern   BITBOARD       endgame_random_b;
extern   BITBOARD       w_rooks_random;
extern   BITBOARD       b_rooks_random;

extern   BITBOARD       clear_mask[65];
extern   BITBOARD       clear_mask_rl45[65];
extern   BITBOARD       clear_mask_rr45[65];
extern   BITBOARD       clear_mask_rl90[65];
extern   BITBOARD       set_mask[65];
extern   BITBOARD       set_mask_rl45[65];
extern   BITBOARD       set_mask_rr45[65];
extern   BITBOARD       set_mask_rl90[65];
extern   BITBOARD       file_mask[8];
extern   BITBOARD       rank_mask[8];
extern   BITBOARD       mask_not_rank8;
extern   BITBOARD       mask_not_rank1;
extern   BITBOARD       right_side_mask[8];
extern   BITBOARD       left_side_mask[8];
extern   BITBOARD       right_side_empty_mask[8];
extern   BITBOARD       left_side_empty_mask[8];
extern   BITBOARD       right_half_mask, left_half_mask;
extern   BITBOARD       mask_abs7_w, mask_abs7_b;
extern   BITBOARD       pawns_cramp_black;
extern   BITBOARD       pawns_cramp_white;
extern   BITBOARD       mask_advance_2_w;
extern   BITBOARD       mask_advance_2_b;
extern   BITBOARD       mask_left_edge;
extern   BITBOARD       mask_right_edge;
extern   BITBOARD       mask_corner_squares;
extern   BITBOARD       promote_mask_w;
extern   BITBOARD       promote_mask_b;
extern   BITBOARD       mask_G2G3;
extern   BITBOARD       mask_B2B3;
extern   BITBOARD       mask_G6G7;
extern   BITBOARD       mask_B6B7;
extern   BITBOARD       mask_F3H3;
extern   BITBOARD       mask_F6H6;
extern   BITBOARD       mask_A3C3;
extern   BITBOARD       mask_A6C6;
extern   BITBOARD       mask_A7H7;
extern   BITBOARD       mask_A2H2;
extern   BITBOARD       center;

extern   BITBOARD       stonewall_white;
extern   BITBOARD       stonewall_black;

extern   BITBOARD       mask_kr_trapped_w[3];
extern   BITBOARD       mask_qr_trapped_w[3];
extern   BITBOARD       mask_kr_trapped_b[3];
extern   BITBOARD       mask_qr_trapped_b[3];

extern   BITBOARD       good_bishop_kw;
extern   BITBOARD       good_bishop_qw;
extern   BITBOARD       good_bishop_kb;
extern   BITBOARD       good_bishop_qb;

extern   BITBOARD       light_squares;
extern   BITBOARD       dark_squares;
extern   BITBOARD       not_rook_pawns;

extern   BITBOARD       plus1dir[65];
extern   BITBOARD       plus7dir[65];
extern   BITBOARD       plus8dir[65];
extern   BITBOARD       plus9dir[65];
extern   BITBOARD       minus1dir[65];
extern   BITBOARD       minus7dir[65];
extern   BITBOARD       minus8dir[65];
extern   BITBOARD       minus9dir[65];

extern   BITBOARD       mask_eptest[64];
#  if !defined(CRAY1)
extern     BITBOARD       mask_1;
extern     BITBOARD       mask_2;
extern     BITBOARD       mask_3;
extern     BITBOARD       mask_4;
extern     BITBOARD       mask_8;
extern     BITBOARD       mask_16;
extern     BITBOARD       mask_32;
extern     BITBOARD       mask_72;
extern     BITBOARD       mask_80;
extern     BITBOARD       mask_85;
extern     BITBOARD       mask_96;
extern     BITBOARD       mask_107;
extern     BITBOARD       mask_108;
extern     BITBOARD       mask_112;
extern     BITBOARD       mask_118;
extern     BITBOARD       mask_120;
extern     BITBOARD       mask_121;
extern     BITBOARD       mask_127;
#  endif
extern   BITBOARD       mask_clear_entry;
#  if !defined(CRAY1)
extern     unsigned char  first_ones[65536];
extern     unsigned char  last_ones[65536];
#  endif
extern   unsigned char  first_ones_8bit[256];
extern   unsigned char  last_ones_8bit[256];
extern   unsigned char  connected_passed[256];

extern   BITBOARD       mask_kingside_attack_w1;
extern   BITBOARD       mask_kingside_attack_w2;
extern   BITBOARD       mask_kingside_attack_b1;
extern   BITBOARD       mask_kingside_attack_b2;
extern   BITBOARD       mask_queenside_attack_w1;
extern   BITBOARD       mask_queenside_attack_w2;
extern   BITBOARD       mask_queenside_attack_b1;
extern   BITBOARD       mask_queenside_attack_b2;

extern   BITBOARD       mask_pawn_protected_b[64];
extern   BITBOARD       mask_pawn_protected_w[64];
extern   BITBOARD       mask_pawn_isolated[64];
extern   BITBOARD       mask_pawn_passed_w[64];
extern   BITBOARD       mask_pawn_passed_b[64];
extern   BITBOARD       mask_promotion_threat_w[64];
extern   BITBOARD       mask_promotion_threat_b[64];
extern   BITBOARD       mask_pawn_connected[64];
extern   BITBOARD       mask_no_pawn_attacks_w[64];
extern   BITBOARD       mask_no_pawn_attacks_b[64];
extern   BITBOARD       mask_a1_corner;
extern   BITBOARD       mask_a8_corner;
extern   BITBOARD       mask_h1_corner;
extern   BITBOARD       mask_h8_corner;
extern   BITBOARD       white_minor_pieces;
extern   BITBOARD       black_minor_pieces;
extern   BITBOARD       white_center_pawns;
extern   BITBOARD       black_center_pawns;
extern   BITBOARD       white_pawn_race_wtm[64];
extern   BITBOARD       white_pawn_race_btm[64];
extern   BITBOARD       black_pawn_race_wtm[64];
extern   BITBOARD       black_pawn_race_btm[64];

extern   BITBOARD       mask_wk_4th, mask_wq_4th, mask_bk_4th, mask_bq_4th;
extern   BITBOARD       mask_wk_5th, mask_wq_5th, mask_bk_5th, mask_bq_5th;
#endif
