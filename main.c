#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <nmmintrin.h>

typedef enum { false, true } bool;
typedef unsigned long u64;
typedef unsigned int u32;
typedef unsigned char u8;

typedef u64 Board;
typedef u8 Position;

#define ENGINE_NAME "goat"
#define ENGINE_AUTHOR "jasper"

Position pos_from_bitboard(Board b);

typedef enum {
    S_WHITE,
    S_BLACK,
} SideId;

typedef struct {
    Board piece[6];
    Board all_pieces;
    Board attacks;
    Board pinned_pieces;
    bool in_check;
    Board check_attack_path;
    bool can_castle_k;
    bool can_castle_q;
    bool lost_k_castle_rights;
    bool lost_q_castle_rights;
    bool did_castle;
} Side;

typedef enum {
    R_PLAYING,
    R_WHITE_WON,
    R_BLACK_WON,
    R_DRAW,
} GameResult;

typedef struct {
    SideId turn;
    Side side[2];
    Board en_passant;
    Board all_pieces;
    GameResult game_result;
    u64 seen_position_hashes[1000];
    int seen_position_hashes_count;
} GameState;

typedef enum {
    P_PAWN,
    P_KNIGHT,
    P_BISHOP,
    P_ROOK,
    P_QUEEN,
    P_KING,
} Piece;

static int PIECE_VALUES[6] = {
    1, // pawn
    3, // knight
    3, // bishop
    5, // rook
    9, // queen
    0, // king
};

typedef enum {
    PROMO_NONE,
    PROMO_QUEEN,
    PROMO_ROOK,
    PROMO_BISHOP,
    PROMO_KNIGHT,
} Promotion;

typedef enum {
    CASTLE_NONE,
    CASTLE_Q,
    CASTLE_K,
} Castle;

typedef union {
    u64 raw;
    struct {
        u8 piece;
        Position from;
        Position to;
        u8 promotion;
        u8 is_en_passant;
        u8 castle;
    } data;
} Move;

typedef struct {
    Move* moves;
    int length;
} MoveList;

typedef struct {
    Move move;
    float eval;
} EvaluatedMove;

const Board W_CASTLE_MASK_K = 0x60;
const Board W_CASTLE_MASK_Q = 0xe;
const Board B_CASTLE_MASK_K = 0x6000000000000000;
const Board B_CASTLE_MASK_Q = 0xe00000000000000;

Board _side_pieces(SideId side, GameState* gs) {
    Board b = 0;
    for (int i = 0; i < 6; i++) {
        b |= gs->side[side].piece[i];
    }
    return b;
}

u64 hash_position(GameState* gs) {
    u64 hash = 0xc0ffeeUL;
    for (u64 i = 0UL; i < 6UL; i++) {
        hash ^= gs->side[S_WHITE].piece[i];
        hash += i;
        hash ^= gs->side[S_BLACK].piece[i];
        hash += i;
    }

    hash += gs->turn;
    hash ^= gs->en_passant * 0xdecafbadUL;
    hash ^= gs->side[S_WHITE].lost_k_castle_rights * 222UL;
    hash ^= gs->side[S_WHITE].lost_q_castle_rights * 444UL;
    hash ^= gs->side[S_BLACK].lost_k_castle_rights * 666UL;
    hash ^= gs->side[S_BLACK].lost_q_castle_rights * 777UL;

    return hash;
}

void print_bitboard(Board board) {
    for (int row = 7; row >= 0; row--) {
        for (int col = 0; col < 8; col++) {
            printf("%lu", ((board >> (row * 8 + col)) & 1UL));
            if (col != 7) {
                printf(" ");
            }
        }
        printf("\n");
    }
}

void write_bitboard_to_text_buf(char* buf[64], Board board, char* icon) {
    for (int row = 7; row >= 0; row--) {
        for (int col = 0; col < 8; col++) {
            if ((board >> (row * 8 + col)) & 1UL) {
                int idx = (7 - row) * 8 + col;

                assert(buf[idx] == 0);

                buf[idx] = icon;
            }
        }
    }
}

int row_from_pos(Position pos) {
    return pos / 8;
}

int col_from_pos(Position pos) {
    return pos % 8;
}

Position pos_from_row_and_col(int row, int col) {
    return row * 8 + col;
}

Board pos_to_bitboard(Position pos) {
    return 1UL << pos;
}

Position notation_to_pos(char* str) {
    assert(((str[0] >= 'a' && str[0] <= 'h')
        || (str[0] >= 'A' && str[0] <= 'H'))
        && str[1] >= '1' && str[1] <= '8');

    bool capital = str[0] >= 'A' || str[0] <= 'H';

    int col = str[0] - (capital ? 'a' : 'A');
    int row = str[1] - '1';

    return pos_from_row_and_col(row, col);
}

bool position_is_legal(Position pos) {
    return pos >= 0 && pos < 64;
}

void pos_to_notation(Position pos, char* buf) {
    assert(position_is_legal(pos));

    int row = row_from_pos(pos);
    int col = col_from_pos(pos);

    buf[0] = col + 'a';
    buf[1] = row + '1';
    buf[2] = 0;
}

char promotion_to_notation(Promotion promo) {
    switch (promo) {
        case PROMO_BISHOP:
            return 'b';
        case PROMO_KNIGHT:
            return 'n';
        case PROMO_ROOK:
            return 'r';
        case PROMO_QUEEN:
            return 'q';
        default:
            assert(false);
    }
}

void move_to_notation(Move move, char* buf, SideId side) {
    // if (move.data.castle != CASTLE_NONE) {
    //     if (move.data.castle == CASTLE_K) {
    //         (void)memcpy(buf, "O-O", 4);
    //     } else if (move.data.castle == CASTLE_Q) {
    //         (void)memcpy(buf, "O-O-O", 6);
    //     }

    //     return;
    // }

    if (move.data.castle != CASTLE_NONE) {
        char* castle;
        if (move.data.castle == CASTLE_K) {
            castle = side == S_WHITE ? "e1g1" : "e8g8";
        } else if (move.data.castle == CASTLE_Q) {
            castle = side == S_WHITE ? "e1c1" : "e8c8";
        }

        (void)memcpy(buf, castle, 5);

        return;
    }

    (void)pos_to_notation(move.data.from, buf);
    (void)pos_to_notation(move.data.to, buf + 2);
    int len = 4;

    if (move.data.promotion != PROMO_NONE) {
        assert(move.data.piece == P_PAWN);
        buf[len++] = promotion_to_notation(move.data.promotion);
    }

    buf[len] = 0;
}

Piece piece_at(Position pos, GameState* gs) {
    Board b = pos_to_bitboard(pos);

    for (int i = 0; i < 6; i++) {
        if (((gs->side[S_WHITE].piece[i] & b)
            | (gs->side[S_BLACK].piece[i] & b)) != 0) {

            return i;
        }
    }

    assert(false);
}

Move notation_to_move(char* buf, GameState* gs) {
    Move move = { 0 };

    // if (strcmp(buf, "O-O-O") == 0) {
    //     move.data.castle = CASTLE_Q;
    //     return move;
    // }
    // if (strcmp(buf, "O-O") == 0) {
    //     move.data.castle = CASTLE_K;
    //     return move;
    // }

    move.data.from = notation_to_pos(buf);
    move.data.to = notation_to_pos(buf + 2);

    if (buf[4] != 0) {
        switch (buf[4]) {
            case 'b':
                move.data.promotion = PROMO_BISHOP;
                break;
            case 'n':
                move.data.promotion = PROMO_KNIGHT;
                break;
            case 'r':
                move.data.promotion = PROMO_ROOK;
                break;
            case 'q':
                move.data.promotion = PROMO_QUEEN;
                break;
            default:
                assert(false);
        }
    }

    move.data.piece = piece_at(move.data.from, gs);

    if (move.data.piece == P_KING) {
        if (move.data.from == notation_to_pos("e1")) {
            if (move.data.to == notation_to_pos("g1")) {
                move.data.castle = CASTLE_K;
            } else if (move.data.to == notation_to_pos("c1")) {
                move.data.castle = CASTLE_Q;
            }
        } else if (move.data.from == notation_to_pos("e8")) {
            if (move.data.to == notation_to_pos("g8")) {
                move.data.castle = CASTLE_K;
            } else if (move.data.to == notation_to_pos("c8")) {
                move.data.castle = CASTLE_Q;
            }
        }
    }

    return move;
}

typedef struct {
    int length;
    Position* positions;
} PositionList;

#define sq(s) pos_to_bitboard(notation_to_pos(s))

void position_offset(Position pos, int row, int col, PositionList* out) {
    int row_ = (int)row_from_pos(pos);
    int col_ = (int)col_from_pos(pos);

    row_ += row;
    col_ += col;

    if (row_ < 0 || col_ < 0 || row_ > 7 || col_ > 7) {
        return;
    }

    out->positions[out->length++] = pos_from_row_and_col(row_, col_);
}

Board position_list_to_bitboard(PositionList* list) {
    Board b = 0;
    for (int i = 0; i < list->length; i++) {
        b |= pos_to_bitboard(list->positions[i]);
    }
    return b;
}

int set_bits_count(Board value) {
    return _mm_popcnt_u64(value);
}

int bitscan_reverse(Board value) {
    return __builtin_ctzll(value);
}

void bitboard_to_position_list(Board b, PositionList* out) {
    int pos_count = set_bits_count(b);

    while (b != 0) {
        int pos = bitscan_reverse(b);
        out->positions[out->length++] = pos;
        b &= ~(1UL << pos);
    }
}

typedef struct {
    Board all_moves;
    Board en_passant_moves;
} PawnMoves;

#define CHECK_PINS Board __pos_mask = pos_to_bitboard(pos); \
    Board __pinned_mask = gs->side[gs->turn].pinned_pieces; \
    if ((__pos_mask & __pinned_mask) != 0) { \
        return 0; \
    }

void assert_piece_exists(Piece p, Position pos, GameState* gs) {
    assert(position_is_legal(pos));
    assert(gs->side[gs->turn].piece[p] & pos_to_bitboard(pos));
}

Board pawn_attacks(Position pos, GameState* gs) {
    // NOTE: This does not include en passant. As the attacks bitboard
    //       is mainly used to detect checks it was not needed.

    Position moves[2] = { 0 };
    PositionList moves_list = { 0 };
    moves_list.positions = moves;

    int y_dir = gs->turn == S_WHITE ? 1 : -1;

    position_offset(pos, y_dir, -1, &moves_list);
    position_offset(pos, y_dir, 1, &moves_list);

    return position_list_to_bitboard(&moves_list);
}

PawnMoves pawn_legal_moves(Position pos, GameState* gs) {
    Board moves = 0;
    Board en_passant_moves = 0;

    SideId side_id = gs->turn;
    SideId op_side = !side_id;

    Board pawn_mask = pos_to_bitboard(pos);
    Board pinned_mask = gs->side[gs->turn].pinned_pieces;
    if ((pawn_mask & pinned_mask) != 0) {
        PawnMoves pm = { 0 };
        return pm;
    }

    assert(gs->side[side_id].piece[P_PAWN] & pawn_mask);

    Board board_all = gs->all_pieces;
    Board board_op = gs->side[op_side].all_pieces;

    Board attacks = pawn_attacks(pos, gs);

    moves |= board_op & pawn_attacks(pos, gs);

    Position advances[2] = { 0 };
    PositionList advances_list = { 0 };
    advances_list.positions = advances;

    int y_dir = gs->turn == S_WHITE ? 1 : -1;

    (void)position_offset(pos, y_dir, 0, &advances_list);

    int row = row_from_pos(pos);
    if (row == (side_id == S_WHITE ? 1 : 6)) {
        assert(advances_list.length == 1);


        Board advance_mask = pos_to_bitboard(advances_list.positions[0]);

        if ((advance_mask & gs->all_pieces) == 0) {
            (void)position_offset(pos, y_dir * 2, 0, &advances_list);
        }
    }

    moves |= ~board_all & position_list_to_bitboard(&advances_list);

    Position en_passant_targets[2] = { 0 };
    PositionList en_passant_targets_list = { 0 };
    en_passant_targets_list.positions = en_passant_targets;

    (void)position_offset(pos, 0, -1, &en_passant_targets_list);
    (void)position_offset(pos, 0, 1, &en_passant_targets_list);

    Board en_passants = gs->en_passant & board_op
        & position_list_to_bitboard(&en_passant_targets_list);

    if (en_passants != 0) {
        PositionList ml = { 0 };
        Position move_list[64] = { 0 };
        ml.positions = move_list;

        bitboard_to_position_list(en_passants, &ml);

        for (int i = 0; i < ml.length; i++) {
            Position p = ml.positions[i];

            Position en_passant[1] = { 0 };
            PositionList en_passant_list = { 0 };
            en_passant_list.positions = en_passant;

            (void)position_offset(p, y_dir, 0, &en_passant_list);

            if (en_passant_list.length > 0) {
                moves |= pos_to_bitboard(en_passant_list.positions[0]);
                en_passant_moves |= pos_to_bitboard(en_passant_list.positions[0]);
            }
        }
    }

    if (gs->side[gs->turn].in_check) {
        moves &= gs->side[gs->turn].check_attack_path;
    }

    moves &= ~(gs->side[S_WHITE].piece[P_KING] | gs->side[S_BLACK].piece[P_KING]);

    PawnMoves pm = { 0 };
    pm.all_moves = moves;
    pm.en_passant_moves = en_passant_moves;

    return pm;
}

static Board KNIGHT_MOVES_LUT[64];
static Board KING_MOVES_LUT[64];

#define add_move(x, y) position_offset(pos, x, y, &move_list)

void generate_knight_LUT() {
    for (Position pos = 0; pos < 64; pos++) {
        Position moves[8] = { 0 };
        PositionList moves_list = { 0 };
        moves_list.positions = moves;

        position_offset(pos, 2, 1, &moves_list);
        position_offset(pos, 2, -1, &moves_list);
        position_offset(pos, -2, 1, &moves_list);
        position_offset(pos, -2, -1, &moves_list);
        position_offset(pos, 1, 2, &moves_list);
        position_offset(pos, 1, -2, &moves_list);
        position_offset(pos, -1, 2, &moves_list);
        position_offset(pos, -1, -2, &moves_list);

        KNIGHT_MOVES_LUT[pos] = position_list_to_bitboard(&moves_list);
    }
}

void generate_king_LUT() {
    for (Position pos = 0; pos < 64; pos++) {
        Position moves[8] = { 0 };
        PositionList move_list = { 0 };
        move_list.positions = moves;

        add_move(1, 1);
        add_move(0, 1);
        add_move(-1, 1);
        add_move(-1, 0);
        add_move(-1, -1);
        add_move(1, -1);
        add_move(0, -1);
        add_move(1, 0);

        KING_MOVES_LUT[pos] = position_list_to_bitboard(&move_list);
    }
}

Board knight_attacks(Position pos, GameState* gs) {
    assert_piece_exists(P_KNIGHT, pos, gs);

    return KNIGHT_MOVES_LUT[pos];
}


Board knight_legal_moves(Position pos, GameState* gs) {
    assert_piece_exists(P_KNIGHT, pos, gs);

    CHECK_PINS

    Board moves = knight_attacks(pos, gs);
    moves &= ~gs->side[gs->turn].all_pieces;

    if (gs->side[gs->turn].in_check) {
        moves &= gs->side[gs->turn].check_attack_path;
    }

    moves &= ~(gs->side[S_WHITE].piece[P_KING] | gs->side[S_BLACK].piece[P_KING]);

    return moves;
}

Board king_attacks(Position pos, GameState* gs) {
    assert_piece_exists(P_KING, pos, gs);

    Board moves = KING_MOVES_LUT[pos];
    moves &= ~gs->side[!gs->turn].attacks;

    // HACK: The king's attacked squares weren't in the attack bitboard.
    //       Probably should be addressed properly.
    Position op_king_pos = pos_from_bitboard(gs->side[!gs->turn].piece[P_KING]);
    moves &= ~KING_MOVES_LUT[op_king_pos];

    return moves;
}

typedef struct {
    int x;
    int y;
} PosOffset;

Board sliding_piece_attacks(Position pos, GameState* gs, const PosOffset* offsets, int offset_dirs_count) {
    Position moves[128] = { 0 };
    PositionList move_list = { 0 };
    move_list.positions = moves;
    int pre_len = move_list.length - 1;

    int x, y;
    for (int dir = 0; dir < offset_dirs_count; dir++) {
        x = offsets[dir].x;
        y = offsets[dir].y;

        for (int i = 1; true; i++) {
            position_offset(pos, i * x, i * y, &move_list);

            if (move_list.length == pre_len) {
                break;
            }
            pre_len = move_list.length;

            Position p = move_list.positions[move_list.length - 1];

            Board pieces = gs->all_pieces &
                ~gs->side[!gs->turn].piece[P_KING];

            if (pieces & pos_to_bitboard(p)) {
                break;
            }
        }
    }

    return position_list_to_bitboard(&move_list);
}

Board king_legal_moves(Position pos, GameState* gs) {
    assert_piece_exists(P_KING, pos, gs);

    Board moves = king_attacks(pos, gs);
    moves &= ~gs->side[gs->turn].all_pieces;

    moves &= ~(gs->side[S_WHITE].piece[P_KING] | gs->side[S_BLACK].piece[P_KING]);

    return moves;
}

Board rook_attacks(Position pos, GameState* gs) {
    const PosOffset offsets[4] = {
        { 1, 0 },
        { -1, 0 },
        { 0, 1 },
        { 0, -1 },
    };

    return sliding_piece_attacks(pos, gs, offsets, 4);
}

Board rook_legal_moves(Position pos, GameState* gs) {
    assert_piece_exists(P_ROOK, pos, gs);

    CHECK_PINS

    Board moves = rook_attacks(pos, gs);
    moves &= ~gs->side[gs->turn].all_pieces;

    if (gs->side[gs->turn].in_check) {
        moves &= gs->side[gs->turn].check_attack_path;
    }

    moves &= ~(gs->side[S_WHITE].piece[P_KING] | gs->side[S_BLACK].piece[P_KING]);

    return moves;
}

Board bishop_attacks(Position pos, GameState* gs) {
    const PosOffset offsets[4] = {
        { 1, 1 },
        { -1, 1 },
        { 1, -1 },
        { -1, -1 },
    };

    return sliding_piece_attacks(pos, gs, offsets, 4);
}

Board bishop_legal_moves(Position pos, GameState* gs) {
    assert_piece_exists(P_BISHOP, pos, gs);

    CHECK_PINS

    Board moves = bishop_attacks(pos, gs);
    moves &= ~gs->side[gs->turn].all_pieces;

    if (gs->side[gs->turn].in_check) {
        moves &= gs->side[gs->turn].check_attack_path;
    }

    moves &= ~(gs->side[S_WHITE].piece[P_KING] | gs->side[S_BLACK].piece[P_KING]);

    return moves;
}

Board queen_attacks(Position pos, GameState* gs) {
    assert_piece_exists(P_QUEEN, pos, gs);

    return bishop_attacks(pos, gs) | rook_attacks(pos, gs);
}


Board queen_legal_moves(Position pos, GameState* gs) {
    assert_piece_exists(P_QUEEN, pos, gs);

    CHECK_PINS

    Board moves = queen_attacks(pos, gs);
    moves &= ~gs->side[gs->turn].all_pieces;

    if (gs->side[gs->turn].in_check) {
        moves &= gs->side[gs->turn].check_attack_path;
    }

    moves &= ~(gs->side[S_WHITE].piece[P_KING] | gs->side[S_BLACK].piece[P_KING]);

    return moves;
}

// HACK: Thank you en passant... I hate you.
static Board EN_PASSANT_MASK_LAST_GENERATED = 0;

Board legal_moves_of_piece(GameState* gs, Piece piece, Position pos) {
    switch (piece) {
        case P_PAWN: {
            PawnMoves pm = pawn_legal_moves(pos, gs);
            EN_PASSANT_MASK_LAST_GENERATED = pm.en_passant_moves;
            return pm.all_moves;
        }
        case P_KNIGHT: return knight_legal_moves(pos, gs);
        case P_BISHOP: return bishop_legal_moves(pos, gs);
        case P_ROOK: return rook_legal_moves(pos, gs);
        case P_QUEEN: return queen_legal_moves(pos, gs);
        case P_KING: return king_legal_moves(pos, gs);
        default: assert(false);
    }
}

bool move_is_legal(GameState* gs, Move move) {
    if (move.data.castle != CASTLE_NONE) {
        if (move.data.castle == CASTLE_Q) {
            return gs->side[gs->turn].can_castle_q;
        }
        if (move.data.castle == CASTLE_K) {
            return gs->side[gs->turn].can_castle_k;
        }
        assert(false);
    }

    if (!position_is_legal(move.data.to)) {
        return false;
    }

    Board to = pos_to_bitboard(move.data.to);
    return (to & legal_moves_of_piece(gs, move.data.piece, move.data.from)) != 0;
}

Board piece_attacks(GameState* gs, Piece piece, Position pos) {
    switch (piece) {
        case P_PAWN: return pawn_attacks(pos, gs);
        case P_KNIGHT: return knight_attacks(pos, gs);
        case P_BISHOP: return bishop_attacks(pos, gs);
        case P_ROOK: return rook_attacks(pos, gs);
        case P_QUEEN: return queen_attacks(pos, gs);
        case P_KING: return king_attacks(pos, gs);
        default: assert(false);
    }
}

float evaluate(GameState* gs);

void print_game_state(GameState* gs, PositionList* highlights) {
    char* text_buf[64] = { 0 };

    #define ADD_PIECES(p, w_icon, b_icon) \
        write_bitboard_to_text_buf(text_buf, gs->side[S_WHITE].piece[p], w_icon); \
        write_bitboard_to_text_buf(text_buf, gs->side[S_BLACK].piece[p], b_icon);

    ADD_PIECES(P_PAWN, "♙", "♟︎");
    ADD_PIECES(P_BISHOP, "♗", "♝");
    ADD_PIECES(P_KNIGHT, "♘", "♞");
    ADD_PIECES(P_ROOK, "♖", "♜");
    ADD_PIECES(P_QUEEN, "♕", "♛");
    ADD_PIECES(P_KING, "♔", "♚");

    printf("\e[0;30m");
    for (int i = 0; i < 64; i++) {
        if (i % 8 == 0) {
            if (i != 0) printf("\n");

            printf("\e[0m%d ", 8 - (i / 8));
        }

        #define PIECE_COL "30"
        #define B_SQUARE_COL "41"
        #define W_SQUARE_COL "47"
        #define HIGHLIGHT_COL "43"

        bool white_square = (((i / 8) % 2) + i) % 2;
        if (white_square) {
            printf("\e[0;" W_SQUARE_COL ";" PIECE_COL "m");
        } else {
            printf("\e[0;" B_SQUARE_COL ";" PIECE_COL "m");
        }

        if (highlights != NULL) {
            for (int j = 0; j < highlights->length; j++) {
                int pos = highlights->positions[j];
                int row = row_from_pos(pos);
                int col = col_from_pos(pos);
                int actual_pos = (7 - row) * 8 + col;

                if (actual_pos == i) {
                    printf("\e[0;" HIGHLIGHT_COL ";" PIECE_COL "m");
                }
            }
        }

        printf("%s", text_buf[i] != 0 ? text_buf[i] :  " ");
        printf(" ");
    }
    printf("\e[0m\n  A B C D E F G H\n");
    printf("Position static eval: %f\n\n", evaluate(gs));
}

Piece map_promo_to_piece_id(Promotion promo_id) {
    switch (promo_id) {
        case PROMO_BISHOP:
            return P_BISHOP;
        case PROMO_KNIGHT:
            return P_KNIGHT;
        case PROMO_ROOK:
            return P_ROOK;
        case PROMO_QUEEN:
            return P_QUEEN;
        default:
            assert(false);
    }
}

void handle_pawn(GameState* gs, Move move) {
    Board from = pos_to_bitboard(move.data.from);
    Board to = pos_to_bitboard(move.data.to);

    SideId side_id = gs->turn;
    SideId op_side = !side_id;

    Piece piece = move.data.piece;

    if (move.data.is_en_passant) {
        int y_dir = side_id == S_WHITE ? 1 : -1;

        Position en_passant_target[1] = { 0 };
        PositionList en_passant_target_list = { 0 };
        en_passant_target_list.positions = en_passant_target;

        (void)position_offset(move.data.to, -y_dir, 0, &en_passant_target_list);

        Board en_passant_mask = position_list_to_bitboard(&en_passant_target_list);

        assert(gs->side[op_side].piece[P_PAWN] & en_passant_mask);

        gs->side[op_side].piece[P_PAWN] &= ~en_passant_mask;
    }

    if (move.data.promotion != PROMO_NONE) {
        assert(row_from_pos(move.data.to) == (side_id == S_WHITE ? 7 : 0));

        int promo_id = map_promo_to_piece_id(move.data.promotion);

        gs->side[side_id].piece[promo_id] |= to;
        gs->side[side_id].piece[P_PAWN] &= ~to;
    } else {
        assert(row_from_pos(move.data.to) != (side_id == S_WHITE ? 7 : 0));
    }

    int row_i = row_from_pos(move.data.from);
    int row_f = row_from_pos(move.data.to);

    int ep_i = side_id == S_WHITE ? 1 : 6;
    int ep_f = side_id == S_WHITE ? 3 : 4;

    if (row_i == ep_i && row_f == ep_f) {
        gs->en_passant |= to;
    }
}

PositionList move_list_to_position_list(MoveList* ml) {
    PositionList pl = { 0 };
    pl.positions = malloc(sizeof(Position) * ml->length);

    for (int i = 0; i < ml->length; i++) {
        pl.positions[pl.length++] = ml->moves[i].data.to;
    }

    return pl;
}

void update_all_piece_stores(GameState* gs) {
    gs->side[S_WHITE].all_pieces = _side_pieces(S_WHITE, gs);
    gs->side[S_BLACK].all_pieces = _side_pieces(S_BLACK, gs);
    gs->all_pieces = gs->side[S_WHITE].all_pieces | gs->side[S_BLACK].all_pieces;
}

Board get_side_attacks(GameState* gs, SideId side) {
    Board b = 0;

    for (Piece p = 0; p < 6; p++) {
        PositionList pl = { 0 };
        Position positions[64];
        pl.positions = positions;

        (void)bitboard_to_position_list(gs->side[side].piece[p], &pl);

        // HACK: I can't be bothered to make this good.
        SideId tmp = gs->turn;
        gs->turn = side;

        for (int i = 0; i < pl.length; i++) {
            b |= piece_attacks(gs, p, pl.positions[i]);
        }

        gs->turn = tmp;
    }

    return b;
}

void add_all_promo_variants(GameState* gs, Move move, MoveList* out) {
    assert(move.data.piece == P_PAWN);

    move.data.promotion = PROMO_QUEEN;
    out->moves[out->length++] = move;
    move.data.promotion = PROMO_KNIGHT;
    out->moves[out->length++] = move;
    move.data.promotion = PROMO_ROOK;
    out->moves[out->length++] = move;
    move.data.promotion = PROMO_BISHOP;
    out->moves[out->length++] = move;
}

bool move_should_be_promo(Move* move) {
    int row = row_from_pos(move->data.to);
    return move->data.piece == P_PAWN && (row == 0 || row == 7);
}

void check_if_piece_pins(GameState* gs, Board* pins,
    Position blocker_pos, Position p, int dir) {

    Piece piece_type = piece_at(p, gs);

    switch (piece_type) {
        case P_ROOK: {
            if ((dir + 1) % 2 == 0) {
                *pins |= pos_to_bitboard(blocker_pos);
            }
            break;
        }
        case P_BISHOP: {
            if (dir % 2 == 0) {
                *pins |= pos_to_bitboard(blocker_pos);
            }
            break;
        }
        case P_QUEEN: {
            *pins |= pos_to_bitboard(blocker_pos);
            break;
        }
        default: break;
    }
}

Board get_pins(GameState* gs, SideId side) {
    Position king_pos = pos_from_bitboard(gs->side[side].piece[P_KING]);
    int king_row = row_from_pos(king_pos);
    int king_col = col_from_pos(king_pos);

    PosOffset dirs[8] = {
        { 1, 1 },
        { 1, 0 },
        { 1, -1 },
        { 0, -1 },
        { -1, -1 },
        { -1, 0 },
        { -1, 1 },
        { 0, 1 },
    };

    Board pins = 0;

    for (int dir = 0; dir < 8; dir++) {
        Position blocker_pos = 65;
        for (int dist = 1; true; dist++) {
            int row = king_row + dirs[dir].y * dist;
            int col = king_col + dirs[dir].x * dist;
            if (row < 0 || row > 7 || col < 0 || col > 7) {
                break;
            }

            Position p = pos_from_row_and_col(row, col);

            Board mask = pos_to_bitboard(p);

            if ((gs->side[side].all_pieces & mask) != 0) {
                if (blocker_pos == 65) {
                    blocker_pos = p;
                    continue;
                } else {
                    break;
                }
            } else if ((gs->side[!side].all_pieces & mask) != 0) {
                if (blocker_pos != 65) {
                    check_if_piece_pins(gs, &pins, blocker_pos, p, dir);
                }
                break;
            }
        }
    }

    return pins;
}

void get_all_legal_moves(GameState* gs, MoveList* out) {
    // TODO: Optimise + refactor
    for (Piece piece_type = 0; piece_type < 6; piece_type++) {
        PositionList pl = { 0 };
        Position positions[64];
        pl.positions = positions;

        (void)bitboard_to_position_list(gs->side[gs->turn].piece[piece_type], &pl);
        for (Position piece_inst = 0; piece_inst < pl.length; piece_inst++) {
            Position from = pl.positions[piece_inst];
            Board legal_moves = legal_moves_of_piece(gs, piece_type, from);

            PositionList ml = { 0 };
            Position moves[64];
            ml.positions = moves;

            (void)bitboard_to_position_list(legal_moves, &ml);

            for (int i = 0; i < ml.length; i++) {
                Move move = { 0 };
                move.data.from = from;
                move.data.to = ml.positions[i];
                move.data.piece = piece_type;

                if (piece_type == P_PAWN) {
                    move.data.is_en_passant = (EN_PASSANT_MASK_LAST_GENERATED
                        & pos_to_bitboard(move.data.to)) != 0;
                }

                if (move_should_be_promo(&move)) {
                    add_all_promo_variants(gs, move, out);
                } else {
                    out->moves[out->length++] = move;
                }
            }
        }
    }

    if (gs->side[gs->turn].can_castle_k) {
        Move move = { 0 };
        move.data.castle = CASTLE_K;
        out->moves[out->length++] = move;
    }
    if (gs->side[gs->turn].can_castle_q) {
        Move move = { 0 };
        move.data.castle = CASTLE_Q;
        out->moves[out->length++] = move;
    }
}

void castle(GameState* gs, Castle c) {
    assert(c != CASTLE_NONE);
    if (c == CASTLE_K) {
        if (gs->turn == S_WHITE) {
            gs->side[gs->turn].piece[P_KING] = sq("g1");
            gs->side[gs->turn].piece[P_ROOK] &= ~sq("h1");
            gs->side[gs->turn].piece[P_ROOK] |= sq("f1");
        } else {
            gs->side[gs->turn].piece[P_KING] = sq("g8");
            gs->side[gs->turn].piece[P_ROOK] &= ~sq("h8");
            gs->side[gs->turn].piece[P_ROOK] |= sq("f8");
        }

    } else if (c == CASTLE_Q) {
        if (gs->turn == S_WHITE) {
            gs->side[gs->turn].piece[P_KING] = sq("c1");
            gs->side[gs->turn].piece[P_ROOK] &= ~sq("a1");
            gs->side[gs->turn].piece[P_ROOK] |= sq("d1");
        } else {
            gs->side[gs->turn].piece[P_KING] = sq("c8");
            gs->side[gs->turn].piece[P_ROOK] &= ~sq("a8");
            gs->side[gs->turn].piece[P_ROOK] |= sq("d8");
        }

    }

    gs->side[gs->turn].lost_q_castle_rights = true;
    gs->side[gs->turn].lost_k_castle_rights = true;
}

Position pos_from_bitboard(Board b) {
    assert(set_bits_count(b) == 1);
    return bitscan_reverse(b);
}

int sign(int n) {
    if (n < 0) return -1;
    if (n > 0) return 1;
    return 0;
}

PosOffset direction(Position pos1, Position pos2) {
    int delta_row = row_from_pos(pos2) - row_from_pos(pos1);
    int delta_col = col_from_pos(pos2) - col_from_pos(pos1);

    return (PosOffset) { sign(delta_row), sign(delta_col) };
}

Board check_attack_path(GameState* gs, SideId s) {
    Board b = 0;

    Board king = gs->side[s].piece[P_KING];
    bool has_seen_one = false;

    for (Piece p = 0; p < 6; p++) {
        PositionList pl = { 0 };
        Position positions[64];
        pl.positions = positions;

        (void)bitboard_to_position_list(gs->side[!s].piece[p], &pl);

        for (int i = 0; i < pl.length; i++) {
            Position pos = pl.positions[i];
            SideId tmp_side = gs->turn;
            gs->turn = !s;
            Board attacks = piece_attacks(gs, p, pos);
            gs->turn = tmp_side;

            if (attacks & king) {
                // If the king is attacked by more than one piece, there is
                // no way to take the attacking piece and thus we just return
                // zero.
                if (has_seen_one) {
                    return 0;
                }
                has_seen_one = true;

                b |= pos_to_bitboard(pos);

                if (p == P_BISHOP || p == P_ROOK || p == P_QUEEN) {
                    Position king_pos = pos_from_bitboard(king);

                    const PosOffset offsets[1] = { direction(king_pos, pos) };

                    b |= sliding_piece_attacks(king_pos, gs, offsets, 1);
                }
            }
        }
    }

    return b;
}

static Board COLS[8];

float material_eval(GameState* gs) {
    float eval = 0;

    for (Piece p = 0; p < 6; p++) {
        int white_count = set_bits_count(gs->side[S_WHITE].piece[p]);
        int black_count = set_bits_count(gs->side[S_BLACK].piece[p]);
        eval += (float)(PIECE_VALUES[p] * (white_count - black_count));
    }

    return eval;
}

int set_bits_in_col(Board b, int col) {
    return set_bits_count(b & COLS[col]);
}

Board get_pawn_attacks(Board pawns, SideId side);

float evaluate_pawn_structure(GameState* gs) {
    float eval = 0.0f;

    Board white = gs->side[S_WHITE].piece[P_PAWN];
    Board black = gs->side[S_BLACK].piece[P_PAWN];

    // Pawn chains
    for (int s = 0; s < 2; s++) {
        float sign = s == 0 ? 1.0f : -1.0f;
        Board pieces = gs->side[s].piece[P_PAWN];
        Board attacks = get_pawn_attacks(pieces, s);
        eval += sign * set_bits_count(attacks & pieces) * 0.5f;
    }

    for (int col = 0; col < 8; col++) {
        // Doubled pawns
        eval += (float)(set_bits_in_col(black, col) - set_bits_in_col(white, col)) * 0.5f;

    }


    return eval;
}

float evaluate_center_control(GameState* gs) {
    float eval = 0.0f;

    const Board center_four = sq("d4") | sq("e4") | sq("d5") | sq("e5");

    const Board outer_center = sq("c6") | sq("d6") | sq("e6") | sq("f6")
        | sq("c5") | sq("f5") | sq("c4") | sq("f4")
        | sq("c3") | sq("d3") | sq("e3") | sq("f3");

    for (int side = 0; side < 2; side++) {
        float sign = side == 0 ? 1.0f : -1.0f;
        int center = set_bits_count((gs->side[side].attacks
            | gs->side[side].all_pieces) & center_four);
        int outer_center = set_bits_count((gs->side[side].attacks
            | gs->side[side].all_pieces) & outer_center);

        eval += sign * ((float)center * 0.2f + (float)outer_center * 0.05f);
    }

    return eval;
}

float evaluate_king_safety(GameState* gs);

float evaluate(GameState* gs) {
    switch (gs->game_result) {
        case R_WHITE_WON: return INFINITY;
        case R_BLACK_WON: return -INFINITY;
        case R_DRAW: return 0;
        default: break;
    }

    float checks = -gs->side[S_WHITE].in_check + gs->side[S_BLACK].in_check;

    float castle_rights = gs->side[S_WHITE].can_castle_k + gs->side[S_WHITE].can_castle_q
        - (gs->side[S_BLACK].can_castle_k + gs->side[S_BLACK].can_castle_q);

    float pins = set_bits_count(gs->side[S_BLACK].pinned_pieces)
        - set_bits_count(gs->side[S_WHITE].pinned_pieces);

    return material_eval(gs) * 1.5
        + castle_rights * 0.4
        + checks * 1.2
        + pins * 0.9
        + evaluate_pawn_structure(gs) * 0.4
        + evaluate_center_control(gs) * 0.9
        + evaluate_king_safety(gs) * 0.6;
}

bool drawn_by_repetition(GameState* gs) {
    int count = 0;
    u64 current_board_hash = hash_position(gs);
    for (int i = 0; i < gs->seen_position_hashes_count; i++) {
        if (current_board_hash == gs->seen_position_hashes[i]) {
            if (++count >= 3) {
                return true;
            }
        }
    }
    return false;
}

typedef struct {
    Piece piece;
    SideId side;
} PieceAndCol;

PieceAndCol piece_from_FEN(char c) {
    SideId s = isupper(c) ? S_WHITE : S_BLACK;
    c = tolower(c);
    switch (c) {
        case 'p': return (PieceAndCol){ P_PAWN, s };
        case 'n': return (PieceAndCol){ P_KNIGHT, s };
        case 'b': return (PieceAndCol){ P_BISHOP, s };
        case 'r': return (PieceAndCol){ P_ROOK, s };
        case 'q': return (PieceAndCol){ P_QUEEN, s };
        case 'k': return (PieceAndCol){ P_KING, s };
        default: return (PieceAndCol){ -1, -1 };
    }
}

GameState parse_FEN(char* fen) {
    GameState gs = { 0 };
    char* error_msg = NULL;

    int cursor = 0;
    for (int row = 7; row >= 0; row--){
        for (int col = 0; col < 8; col++) {
            char c = fen[cursor++];
            PieceAndCol p = piece_from_FEN(c);
            if (p.piece != -1) {
                // Was letter. Add piece to board.
                gs.side[p.side].piece[p.piece] |=
                    pos_to_bitboard(pos_from_row_and_col(row, col));
            } else if (isdigit(c)) {
                col += c - '0' - 1;
                if (col >= 8) {
                    char* msg_template = "invalid number '%c'. row too long";
                    error_msg = malloc(strlen(msg_template) + 1);
                    sprintf(error_msg, msg_template, c);
                    goto err;
                }
            } else {
                char* msg_template = "invalid character '%c'";
                error_msg = malloc(strlen(msg_template) + 1);
                sprintf(error_msg, msg_template, c);
                goto err;
            }
        }

        if (row != 0 && fen[cursor++] != '/') {
            error_msg = "row not terminated by '/'";
            goto err;
        }
    }

    #define CHECK_SPACE if (fen[cursor++] != ' ') { \
        error_msg = "expected space"; \
        goto err; \
    } \

    CHECK_SPACE

    switch (fen[cursor++]) {
        case 'w': gs.turn = S_WHITE; break;
        case 'b': gs.turn = S_BLACK; break;
        default:
            error_msg = "invalid turn. expected 'w' or 'b'";
            goto err;
    }

    CHECK_SPACE

    gs.side[S_WHITE].lost_k_castle_rights = true;
    gs.side[S_WHITE].lost_q_castle_rights = true;
    gs.side[S_BLACK].lost_k_castle_rights = true;
    gs.side[S_BLACK].lost_q_castle_rights = true;

    if (fen[cursor] == '-') {
        cursor++;
    } else {
        for (int i = 0; i < 4; i++) {
            char c = fen[cursor++];

            if (c == ' ') {
                cursor--;
                break;
            }

            switch (c) {
                case 'K': gs.side[S_WHITE].lost_k_castle_rights = false;
                case 'Q': gs.side[S_WHITE].lost_q_castle_rights = false;
                case 'k': gs.side[S_BLACK].lost_k_castle_rights = false;
                case 'q': gs.side[S_BLACK].lost_q_castle_rights = false;
            }
        }
    }

    CHECK_SPACE

    char en_passant[3] = { 0 };
    for (int i = 0; i < 2; i++) {
        char c = fen[cursor++];
        if (c == ' ') {
            cursor--;
            break;
        }
        en_passant[i] = c;
    }

    if (en_passant[0] != '-') {
        if (!isalpha(en_passant[0])
        || !isdigit(en_passant[1])
        || en_passant[2] != 0) {

            char* msg_template = "invalid en passant position '%s'. expected 'a'-'h' followed by '1'-'8'";
            error_msg = malloc(strlen(msg_template) + 2);
            sprintf(error_msg, msg_template, en_passant);
            goto err;
        } else {
            Position pos = notation_to_pos(en_passant);

            // We handle en passant by storing the position of the pawn that
            // can be captured which is different from the position in the FEN
            // which is that which the capturing pawn moves to. Thus we just
            // find the row ourselves and use only the column from the FEN.
            int col = col_from_pos(pos);
            int row = gs.turn == S_WHITE ? 4 : 3;

            int row_from_FEN = row_from_pos(pos);
            if (row_from_FEN != (gs.turn == S_WHITE ? 5 : 2)) {
                char* msg_template = "invalid en passant position '%s'";
                error_msg = malloc(strlen(msg_template) + 2);
                sprintf(error_msg, msg_template, en_passant);
                goto err;
            }

            gs.en_passant = pos_to_bitboard(pos_from_row_and_col(row, col));
        }
    }

    // Halfmove clock and fullmove counter would be parsed here
    // but they are not needed.

    err:
    if (error_msg != NULL) {
        printf("Invalid FEN: %s.\n", error_msg);
        exit(1);
    }

    return gs;
}

void apply_move(GameState* gs, Move move) {
    assert(move_is_legal(gs, move));
    assert(gs->game_result == R_PLAYING);

    gs->en_passant = 0;

    Piece piece = move.data.piece;

    if (move.data.castle != CASTLE_NONE) {
        castle(gs, move.data.castle);
        gs->side[gs->turn].did_castle = true;
    } else {
        // TODO: refactor this into a function
        Board from = pos_to_bitboard(move.data.from);
        Board to = pos_to_bitboard(move.data.to);

        SideId side_id = gs->turn;
        SideId op_side = !side_id;

        assert(gs->side[side_id].piece[piece] & from);

        gs->side[side_id].piece[piece] &= ~from;
        gs->side[side_id].piece[piece] |= to;

        if (gs->side[op_side].all_pieces & to) {
            for (int i = 0; i < 6; i++) {
                gs->side[op_side].piece[i] &= ~to;
            }
        }

        if (piece == P_PAWN) {
            handle_pawn(gs, move);
        }

    }

    update_all_piece_stores(gs);
    // Board update complete.

    // Revoke opponents castling rights if we just moved to
    // either of their rook's starting positions.
    if (!gs->side[!gs->turn].lost_k_castle_rights) {
        Board k_side = gs->turn != S_WHITE
            ? notation_to_pos("h1")
            : notation_to_pos("h8");
        if (move.data.to == k_side) {
            gs->side[!gs->turn].lost_k_castle_rights = true;
        }
    }

    if (!gs->side[!gs->turn].lost_q_castle_rights) {
        Board q_side = gs->turn != S_WHITE
            ? notation_to_pos("a1")
            : notation_to_pos("a8");
        if (move.data.to == q_side) {
            gs->side[!gs->turn].lost_q_castle_rights = true;
        }
    }

    if (piece == P_KING) {
        gs->side[gs->turn].lost_k_castle_rights = true;
        gs->side[gs->turn].lost_q_castle_rights = true;
    } else if (piece == P_ROOK) {
        Board k_side = gs->turn == S_WHITE
            ? notation_to_pos("h1")
            : notation_to_pos("h8");
        Board q_side = gs->turn == S_WHITE
            ? notation_to_pos("a1")
            : notation_to_pos("a8");

        if (move.data.from == k_side) {
            gs->side[gs->turn].lost_k_castle_rights = true;
        } else if (move.data.from == q_side) {
            gs->side[gs->turn].lost_q_castle_rights = true;
        }
    }

    for (SideId s = 0; s < 2; s++) {
        gs->side[s].attacks = get_side_attacks(gs, s);
    }

    for (SideId s = 0; s < 2; s++) {
        gs->side[s].in_check = (gs->side[!s].attacks &
            gs->side[s].piece[P_KING]) != 0;

        if (gs->side[s].in_check) {
            gs->side[s].check_attack_path = check_attack_path(gs, s);
        }
    }

    for (SideId s = 0; s < 2; s++) {
        if (!gs->side[s].in_check) {
            Board q_side_mask = s == S_WHITE ? W_CASTLE_MASK_Q : B_CASTLE_MASK_Q;
            Board k_side_mask = s == S_WHITE ? W_CASTLE_MASK_K : B_CASTLE_MASK_K;

            bool clear_k = (k_side_mask & (gs->all_pieces | gs->side[!s].attacks)) == 0;

            gs->side[s].can_castle_k = clear_k
                && !gs->side[s].lost_k_castle_rights;

            bool clear_q = (q_side_mask & (gs->all_pieces | gs->side[!s].attacks)) == 0;

            gs->side[s].can_castle_q = clear_q
                && !gs->side[s].lost_q_castle_rights;
        } else {
            gs->side[s].can_castle_k = false;
            gs->side[s].can_castle_q = false;
        }
    }

    gs->seen_position_hashes[gs->seen_position_hashes_count++]
        = hash_position(gs);

    if (drawn_by_repetition(gs)) {
        gs->game_result = R_DRAW;
    }

    gs->side[S_WHITE].pinned_pieces = get_pins(gs, S_WHITE);
    gs->side[S_BLACK].pinned_pieces = get_pins(gs, S_BLACK);

    gs->turn = !gs->turn;

    if (gs->side[gs->turn].in_check) {
        // Checkmate
        MoveList ml = { 0 };
        Move moves[128];
        ml.moves = moves;

        get_all_legal_moves(gs, &ml);

        if (ml.length == 0) {
            gs->game_result = gs->turn == S_WHITE ? R_BLACK_WON : R_WHITE_WON;
        }
    }
}

void generate_cols() {
    for (int i = 0; i < 8; i++) {
        COLS[i] = 0x0101010101010101 << i;
    }
}

void generate_LUTs() {
    generate_knight_LUT();
    generate_king_LUT();
    generate_cols();
}

void set_up_board(GameState* gs) {
    gs->side[S_WHITE].piece[P_KNIGHT] = 0x42;
    gs->side[S_WHITE].piece[P_BISHOP] = 0x24;
    gs->side[S_WHITE].piece[P_ROOK] = 0x81;
    gs->side[S_WHITE].piece[P_QUEEN] = 0x08;
    gs->side[S_WHITE].piece[P_KING] = 0x10;
    gs->side[S_WHITE].piece[P_PAWN] = 0xFF00;

    gs->side[S_BLACK].piece[P_KNIGHT] = 0x4200000000000000;
    gs->side[S_BLACK].piece[P_BISHOP] = 0x2400000000000000;
    gs->side[S_BLACK].piece[P_ROOK] = 0x8100000000000000;
    gs->side[S_BLACK].piece[P_QUEEN] = 0x0800000000000000;
    gs->side[S_BLACK].piece[P_KING] = 0x1000000000000000;
    gs->side[S_BLACK].piece[P_PAWN] = 0x00FF000000000000;
}

Board get_pawn_attacks(Board pawns, SideId side) {
    Board attacks = 0;

    PositionList pl = { 0 };
    Position positions[64];
    pl.positions = positions;

    GameState gs = { 0 };
    gs.side[side].piece[P_PAWN] = pawns;
    gs.turn = side;

    (void)bitboard_to_position_list(pawns, &pl);

    for (int i = 0; i < pl.length; i++) {
        Position pos = pl.positions[i];

        attacks |= pawn_attacks(pos, &gs);
    }

    return attacks;
}

float evaluate_king_safety(GameState* gs) {
    float eval = 0.0f;

    Board king_w = gs->side[S_WHITE].piece[P_KING];
    Board king_b = gs->side[S_BLACK].piece[P_KING];

    for (int side = 0; side < 2; side++) {
        float sign = side == S_WHITE ? 1.0f : -1.0f;

        if (gs->side[side].did_castle) {
            eval += sign * 0.5f;

            Position king_pos = pos_from_bitboard(king_w);
            Board pawns = gs->side[side].piece[P_PAWN];
            int pawn_shield = set_bits_count(pawns & KING_MOVES_LUT[king_pos]);
            eval += sign * pawn_shield * 0.6f;
        }
    }

    return eval;
}

typedef struct {
    u64 hash;
    EvaluatedMove move;
    int depth;
} TranspositionTableEntry;

typedef struct {
    TranspositionTableEntry* data;
    u64 capacity;
} TranspositionTable;

TranspositionTable new_tt(u64 capacity) {
    TranspositionTableEntry* data =
        malloc(sizeof(TranspositionTableEntry) * capacity);
    TranspositionTable tt = { data, capacity };
    return tt;
}

TranspositionTableEntry query_tt(TranspositionTable* tt, GameState* gs) {
    u64 hash = hash_position(gs);
    TranspositionTableEntry search_result = tt->data[hash % tt->capacity];

    if (search_result.hash == hash) {
        return search_result;
    } else {
        TranspositionTableEntry te = { 0 };
        return te;
    }
}

void insert_into_tt(TranspositionTable* tt, GameState* gs, EvaluatedMove em, int depth) {
    u64 hash = hash_position(gs);

    // Depth-Preferred
    TranspositionTableEntry existing = tt->data[hash % tt->capacity];
    if (existing.hash == hash && existing.depth > depth) {
        return;
    }

    TranspositionTableEntry entry = { hash, em, depth };

    tt->data[hash % tt->capacity] = entry;
}

bool is_null_move(Move* m) {
    return m->data.from == 0 && m->data.to == 0;
}

EvaluatedMove minimax(int depth, GameState* gs, float alpha, float beta, TranspositionTable* tt) {
    if (depth == 0 || gs->game_result != R_PLAYING) {
        Move mv = { 0 };
        float eval = evaluate(gs);
        EvaluatedMove em = { mv, eval };
        return em;
    }

    MoveList ml = { 0 };
    Move moves[256];
    ml.moves = moves;

    get_all_legal_moves(gs, &ml);

    EvaluatedMove best_move = { 0 };
    best_move.eval = gs->turn == S_WHITE ? -INFINITY : INFINITY;

    bool max = gs->turn == S_WHITE;

    for (int i = 0; i < ml.length; i++) {
        GameState new_gs = *gs;
        apply_move(&new_gs, ml.moves[i]);

        EvaluatedMove em;

        TranspositionTableEntry em_from_tt = query_tt(tt, &new_gs);

        if (!is_null_move(&em_from_tt.move.move) && em_from_tt.depth >= depth - 1) {
            em = em_from_tt.move;
        } else {
            em = minimax(depth - 1, &new_gs, alpha, beta, tt);
        }

        bool move_is_better = max
            ? em.eval > best_move.eval
            : em.eval < best_move.eval;

        if (move_is_better) {
            best_move.eval = em.eval;
            best_move.move = ml.moves[i];
        }

        if (max) {
            alpha = fmaxf(alpha, em.eval);
            if (beta <= alpha) {
                break;
            }
        } else {
            beta = fminf(beta, em.eval);
            if (beta <= alpha) {
                break;
            }
        }

    }

    insert_into_tt(tt, gs, best_move, depth);

    return best_move;
}

EvaluatedMove engine_move(GameState* gs, TranspositionTable* tt) {
    EvaluatedMove em = minimax(6, gs, -INFINITY, INFINITY, tt);

    if (em.eval == INFINITY || em.eval == -INFINITY) {
        // printf("%s has forced mate!\n", em.eval > 0 ? "White" : "Black");

        MoveList ml = { 0 };
        Move moves[128];
        ml.moves = moves;
        get_all_legal_moves(gs, &ml);
        if (ml.length == 0) {
            // printf("Stalemate!\n");
            return (EvaluatedMove) { 0 };
        }
        em.move = ml.moves[0];
    }

    apply_move(gs, em.move);
    return em;
}

void reset_game(GameState* gs) {
    memset(gs, 0, sizeof(GameState));
}

void play_self_loop(GameState* gs, TranspositionTable* tt) {
    for (int move = 0; true; move++) {
        printf("%s to move.\n", gs->turn == S_WHITE ? "White" : "Black");
        printf("Thinking...\n");

        SideId side_to_move = gs->turn;
        EvaluatedMove em = engine_move(gs, tt);

        char move_notation[12];
        move_to_notation(em.move, move_notation, side_to_move);

        printf("Playing move: %s\n", move_notation);
        printf("Eval: %f\n", em.eval);

        PositionList pl = { 0 };
        Position positions[2] = { em.move.data.from, em.move.data.to };
        pl.positions = positions;
        pl.length = 2;

        print_game_state(gs, &pl);

        if (gs->game_result != R_PLAYING) {
            printf("Final Eval: %f\n", evaluate(gs));
            break;
        }

    }
}

void apply_move_list(char* str, GameState* gs) {
    char move_buf[10] = { 0 };
    int move_buf_len = 0;
    while (*(++str) != 0) {
        if (isalnum(*str)) {
            move_buf[move_buf_len++] = *str;
        } else if (*str == ' ' || *str == '\n') {
            if (move_buf_len == 0) continue;

            move_buf[move_buf_len] = 0;

            printf("move: %s\n", move_buf);

            GameState tmp_gs = *gs;

            Move move = notation_to_move(move_buf, &tmp_gs);
            apply_move(gs, move);

            move_buf_len = 0;
        } else {
            assert(false);
        }
    }
}

void UCI_loop(GameState* gs, TranspositionTable* tt) {
    char line[2048];
    while (true) {
        if (fgets(line, sizeof(line), stdin) == NULL) {
            continue;
        }
        if (strncmp(line, "ucinewgame", 10) == 0) {
            (void)reset_game(gs);
            (void)set_up_board(gs);
            (void)update_all_piece_stores(gs);
        } else if (strncmp(line, "uci", 3) == 0) {
            printf("id name %s\n", ENGINE_NAME);
            printf("id author %s\n", ENGINE_AUTHOR);
            printf("uciok\n");
        } else if (strncmp(line, "isready", 7) == 0) {
            printf("readyok\n");
        } else if ((strncmp(line, "stop", 4) == 0) || (strncmp(line, "quit", 4) == 0)) {
            // break;
        } else if (strncmp(line, "position", 8) == 0) {
            char* pos = line + 9;
            if (strncmp(pos, "startpos", 8) == 0) {
                (void)reset_game(gs);
                (void)set_up_board(gs);
                (void)update_all_piece_stores(gs);
                pos += 9;
            } else if (strncmp(pos, "fen", 3) == 0) {
                pos += 4;
                *gs = parse_FEN(pos);
            }

            if (strncmp(pos, "moves", 5) == 0) {
                pos += 5;

                (void)apply_move_list(pos, gs);
            }
        } else if (strncmp(line, "go", 2) == 0) {
            SideId side_to_move = gs->turn;
            EvaluatedMove em = engine_move(gs, tt);

            char move_notation[12];
            move_to_notation(em.move, move_notation, side_to_move);

            printf("bestmove %s\n", move_notation);
        } else if (strncmp(line, "print", 5) == 0) {
            PositionList pl = { 0 };
            Position positions[64];
            pl.positions = positions;

            Board b = get_pins(gs, S_WHITE);

            bitboard_to_position_list(b, &pl);

            print_game_state(gs, &pl);
            printf("%s to move\n", gs->turn == S_WHITE ? "white" : "black");
        } else {
            printf("Unknown command: %s\n", line);
        }
        fflush(stdout);
    }
}

int main(int argc, char* argv[]) {
    GameState gs = { 0 };

    (void)generate_LUTs();

    (void)set_up_board(&gs);

    (void)update_all_piece_stores(&gs);

    TranspositionTable tt = new_tt(1e8);
    // printf("TT capacity: %f GiB\n", (float)(tt.capacity * sizeof(TranspositionTableEntry)) / (float)(1024 * 1024 * 1024));

    #define move(m) apply_move(&gs, notation_to_move(m, &gs))

    // FIXME: This causes a segfault when you try to calculate the next move.
    // gs = parse_FEN("Knnnkbnn/pppppnpp/n1nnnnnn/nqnnrQnn/nnbNPpnn/nnnnnnPn/PPPPnPnP/RNB2B1R b A e3 0 1");

    play_self_loop(&gs, &tt);
    UCI_loop(&gs, &tt);


    return 0;
}
