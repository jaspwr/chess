#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <nmmintrin.h>

#include "psv/psv.h"

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
    Board pins_attack_paths[64];
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

void update_game_state_info(GameState* gs);

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

typedef struct {
    int x;
    int y;
} PosOffset;

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



#define CHECK_PINS(attack_fn) Board __pos_mask = pos_to_bitboard(pos); \
    Board __pinned_mask = gs->side[gs->turn].pinned_pieces; \
    if ((__pos_mask & __pinned_mask) != 0) { \
        Board __pin_attack_path = gs->side[gs->turn].pins_attack_paths[pos]; \
        Board __own_pieces = gs->side[gs->turn].all_pieces; \
        return (attack_fn(pos, gs) & __pin_attack_path) & ~__own_pieces; \
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

    Board board_all = gs->all_pieces;
    Board board_op = gs->side[op_side].all_pieces;

    Board attacks = board_op & pawn_attacks(pos, gs);

    Board pawn_mask = pos_to_bitboard(pos);
    Board pinned_mask = gs->side[gs->turn].pinned_pieces;
    if ((pawn_mask & pinned_mask) != 0) {
        PawnMoves pm = { 0 };
        pm.all_moves = attacks & gs->side[gs->turn].pins_attack_paths[pos];
        return pm;
    }

    assert(gs->side[side_id].piece[P_PAWN] & pawn_mask);

    moves |= pawn_attacks(pos, gs);

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

    moves &= ~gs->side[gs->turn].all_pieces;

    PawnMoves pm = { 0 };
    pm.all_moves = moves;
    pm.en_passant_moves = en_passant_moves;

    return pm;
}

static Board COLS[8];
static Board ROWS[8];

static Board KNIGHT_MOVES_LUT[64];
static Board KING_MOVES_LUT[64];
static Board BISHOP_MOVES_LUT[64];
static Board ROOK_MOVES_LUT[64];

#define ROOK_PERMS 8 * 4096
#define ROOK_SHIFT 15
#define BISHOP_PERMS 8 * 4096
#define BISHOP_SHIFT 15

static Board BISHOP_MAGIC_BOARDS[64][BISHOP_PERMS] = { 0 }; // 256 K
static Board ROOK_MAGIC_BOARDS[64][ROOK_PERMS] = { 0 }; // 2048 K

static u64 BISHOP_MAGIC_NUMBERS[64] = {
    2658911450287794421,
    6852417887120316938,
    1159619772494454267,
    4530306807827729113,
    4184186518776449728,
    6996729517409051523,
    6694277110281440227,
    4367046700827347423,
    5002521922874173148,
    707789975046747826,
    8845803124310741094,
    3053780606037994088,
    782875984623577188,
    1340588251091286675,
    5117580335483048948,
    8042612948621996202,
    2783607372402123321,
    1442455459770969771,
    5692530968041345214,
    4649229338657423104,
    2305795996909174787,
    4611698444337271150,
    4629462408027598048,
    8622995163449815274,
    6456489114168196145,
    510080667540968688,
    4323454183829978828,
    68334489678698,
    3558652225865219584,
    5706513065946585703,
    1786281091850907230,
    8201950827265312549,
    694435915564839760,
    840916925791039369,
    1115610479759747424,
    6880091598734649088,
    9161611439159255552,
    5414701498144979799,
    3359132947749940723,
    2167603366743129587,
    2022505955381231761,
    5998988709168406901,
    3084769873457330128,
    7562712105529095721,
    2634012166121824780,
    8481960309935617736,
    1295637020739438321,
    7389596876633812746,
    4662164641528356395,
    4274287365172863505,
    2646470775556534080,
    825937101719960739,
    3176643519900238103,
    9026745600027081100,
    2125726254840799092,
    7498071283202784097,
    3289870684522899707,
    5005907598103146988,
    7827854030464587121,
    2189585998322548473,
    8686252344764419816,
    2440490461859161454,
    152347237491388123,
    8496541520937575975,
};

static u64 ROOK_MAGIC_NUMBERS[64] = {
    351979748819113472,
    7803309538379299328,
    1736271716617851776,
    8109988271961577632,
    432345414004662826,
    6673893786975006608,
    4474708985453114624,
    8141247327844093598,
    7425962628245966592,
    1642867420846126720,
    2759411911526057344,
    3357996469423967328,
    4521686761654498272,
    5079380718783458248,
    8005694169337838520,
    5553027955233797434,
    4089864033709443328,
    5160404229161359616,
    4389565480264011648,
    6154169681462285184,
    6751583036203832889,
    7784001533825787536,
    7611136035526543800,
    4887443686719849878,
    6633336694756633856,
    3595239369322866432,
    7044805029705048064,
    2281069068459920160,
    4379372414207810190,
    3273077888100893176,
    6836300768995319696,
    6074204129578170796,
    6511605973395510272,
    188959452880595840,
    9223320189913268096,
    5470114901861073408,
    174381527388389152,
    2842330991730432984,
    5162016821703487056,
    6755246923111856388,
    3917853114319556096,
    5621910873506324480,
    7574010561759584256,
    7020725938020357888,
    8045912710459918336,
    1679284573738097840,
    8085835441630863108,
    1723801058847070596,
    1644622125664786944,
    5076042003988832512,
    6561642211069593856,
    4907242536322792960,
    1066234342103656960,
    5723721981445960192,
    7032640680205726208,
    5945373557256219552,
    4643755003622426662,
    4725706427850877098,
    1489248111780252278,
    2766412166700150694,
    8690697145264131078,
    4008559275060228042,
    191737606960205964,
    6096466831532416022,
};

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

void generate_rook_LUT() {
    for (Position pos = 0; pos < 64; pos++) {
        int row = row_from_pos(pos);
        int col = col_from_pos(pos);
        ROOK_MOVES_LUT[pos] = (ROWS[row] | COLS[col]) & ~pos_to_bitboard(pos);
    }
}

void generate_bishop_attack_bitboard(Position pos) {
    BISHOP_MOVES_LUT[pos] = 0;

    const PosOffset offsets[4] = {
        { 1, 1 },
        { -1, 1 },
        { 1, -1 },
        { -1, -1 },
    };

    int row = row_from_pos(pos);
    int col = col_from_pos(pos);

    for (int dir = 0; dir < 4; dir++) {
        for (int dist = 1; dist < 8; dist++) {
            int row_ = row + offsets[dir].y * dist;
            int col_ = col + offsets[dir].x * dist;

            if (row_ < 0 || row_ > 7 || col_ < 0 || col_ > 7) {
                break;
            }

            Position pos_ = pos_from_row_and_col(row_, col_);
            BISHOP_MOVES_LUT[pos] |= pos_to_bitboard(pos_);
        }
    }
}

void generate_bishop_LUT() {
    for (Position pos = 0; pos < 64; pos++) {
        generate_bishop_attack_bitboard(pos);

        assert(BISHOP_MOVES_LUT[pos] != 0);
    }
}

Board knight_attacks(Position pos, GameState* gs) {
    assert_piece_exists(P_KNIGHT, pos, gs);

    return KNIGHT_MOVES_LUT[pos];
}


Board knight_legal_moves(Position pos, GameState* gs) {
    assert_piece_exists(P_KNIGHT, pos, gs);

    CHECK_PINS(knight_attacks)

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

Board sliding_piece_attacks(Position pos, Board occupancies,
    const PosOffset* offsets, int offset_dirs_count) {

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

            if (occupancies & pos_to_bitboard(p)) {
                break;
            }
        }
    }

    return position_list_to_bitboard(&move_list);
}

Board sliding_piece_attacks_from_gs(Position pos, GameState* gs,
    const PosOffset* offsets, int offset_dirs_count) {

    Board occupancies = gs->all_pieces &
        ~gs->side[!gs->turn].piece[P_KING];

    return sliding_piece_attacks(pos, occupancies, offsets, offset_dirs_count);
}

Board king_legal_moves(Position pos, GameState* gs) {
    assert_piece_exists(P_KING, pos, gs);

    Board moves = king_attacks(pos, gs);
    moves &= ~gs->side[gs->turn].all_pieces;

    moves &= ~(gs->side[S_WHITE].piece[P_KING] | gs->side[S_BLACK].piece[P_KING]);

    return moves;
}

static PosOffset ROOK_OFFSETS[4] = {
    { 1, 0 },
    { -1, 0 },
    { 0, 1 },
    { 0, -1 },
};

const PosOffset BISHOP_OFFSETS[4] = {
    { 1, 1 },
    { -1, 1 },
    { 1, -1 },
    { -1, -1 },
};

u64 magic_board_hash(Board occupancies, u64 moves_mask, u64 magic, int shift) {
    occupancies &= moves_mask;
    occupancies *= magic;
    occupancies >>= 64 - shift;
    return occupancies;
}

Board magic_rook_attacks(Position pos, GameState* gs) {
    Board occupancies = gs->all_pieces &
        ~gs->side[!gs->turn].piece[P_KING];

    u64 hash = magic_board_hash(occupancies, ROOK_MOVES_LUT[pos],
        ROOK_MAGIC_NUMBERS[pos], ROOK_SHIFT);

    assert(hash < ROOK_PERMS);
    return ROOK_MAGIC_BOARDS[pos][hash];
}

Board rook_attacks(Position pos, GameState* gs) {
    // return sliding_piece_attacks_from_gs(pos, gs, ROOK_OFFSETS, 4);
    return magic_rook_attacks(pos, gs);
}

Board rook_legal_moves(Position pos, GameState* gs) {
    assert_piece_exists(P_ROOK, pos, gs);

    CHECK_PINS(rook_attacks)

    Board moves = rook_attacks(pos, gs);
    moves &= ~gs->side[gs->turn].all_pieces;

    if (gs->side[gs->turn].in_check) {
        moves &= gs->side[gs->turn].check_attack_path;
    }

    moves &= ~(gs->side[S_WHITE].piece[P_KING] | gs->side[S_BLACK].piece[P_KING]);

    return moves;
}

Board magic_bishop_attacks(Position pos, GameState* gs) {
    // King is excluded because it is impossible to block
    // with the king and otherwise the king can just move
    // backwards and be out of check as this attack path
    // will go up to the king and stop if he is not excluded.
    Board occupancies = gs->all_pieces &
        ~gs->side[!gs->turn].piece[P_KING];

    u64 hash = magic_board_hash(occupancies, BISHOP_MOVES_LUT[pos],
        BISHOP_MAGIC_NUMBERS[pos], BISHOP_SHIFT);

    assert(hash < BISHOP_PERMS);
    return BISHOP_MAGIC_BOARDS[pos][hash];
}

Board bishop_attacks(Position pos, GameState* gs) {
    return magic_bishop_attacks(pos, gs);
    // return sliding_piece_attacks_from_gs(pos, gs, BISHOP_OFFSETS, 4);
}


Board bishop_legal_moves(Position pos, GameState* gs) {
    assert_piece_exists(P_BISHOP, pos, gs);

    CHECK_PINS(bishop_attacks)

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

    CHECK_PINS(queen_attacks)

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

Board check_if_piece_pins(GameState* gs,
    Position blocker_pos, Position p, int dir) {

    Piece piece_type = piece_at(p, gs);

    switch (piece_type) {
        case P_ROOK: {
            if ((dir + 1) % 2 == 0) {
                return pos_to_bitboard(blocker_pos);
            }
            break;
        }
        case P_BISHOP: {
            if (dir % 2 == 0) {
                return pos_to_bitboard(blocker_pos);
            }
            break;
        }
        case P_QUEEN: {
            return pos_to_bitboard(blocker_pos);
        }
        default: return 0;
    }

    return 0;
}

void get_pins(GameState* gs, SideId side) {
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

    Board attack_path;

    gs->side[side].pinned_pieces = 0;

    for (int dir = 0; dir < 8; dir++) {
        Position blocker_pos = 65;
        attack_path = 0;
        for (int dist = 1; true; dist++) {
            int row = king_row + dirs[dir].y * dist;
            int col = king_col + dirs[dir].x * dist;
            if (row < 0 || row > 7 || col < 0 || col > 7) {
                break;
            }

            Position p = pos_from_row_and_col(row, col);

            Board mask = pos_to_bitboard(p);
            attack_path |= mask;

            if ((gs->side[side].all_pieces & mask) != 0) {
                if (blocker_pos == 65) {
                    blocker_pos = p;
                    continue;
                } else {
                    break;
                }
            } else if ((gs->side[!side].all_pieces & mask) != 0) {
                if (blocker_pos != 65) {
                    Board pinned_blocker = check_if_piece_pins(gs, blocker_pos, p, dir);
                    if (pinned_blocker != 0) {
                        gs->side[side].pins_attack_paths[blocker_pos] = attack_path;
                        gs->side[side].pinned_pieces |= pinned_blocker;
                    }
                }
                break;
            }
        }
    }
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

                    b |= sliding_piece_attacks_from_gs(king_pos, gs, offsets, 1);
                }
            }
        }
    }

    return b;
}

float evaluate_material(GameState* gs) {
    float eval = 0;

    for (Piece p = 0; p < 6; p++) {
        int white_count = set_bits_count(gs->side[S_WHITE].piece[p]);
        int black_count = set_bits_count(gs->side[S_BLACK].piece[p]);
        eval += (float)(PIECE_VALUES[p] * (white_count - black_count));
    }

    return eval;
}

Position invert_board(Position pos) {
    int row = row_from_pos(pos);
    int col = col_from_pos(pos);
    return pos_from_row_and_col(7 - row, col);
}

float lerp(float a, float b, float t) {
    return a + (b - a) * t;
}

float evaluate_piece_square_tables(GameState* gs) {
    float eval = 0.0f;

    for (SideId side = 0; side < 2; side++) {
        float sign = side == S_WHITE ? 1.0f : -1.0f;

        float pieces_on_board = set_bits_count(gs->all_pieces) / 32.0f;

        for (Piece p = 0; p < 6; p++) {
            PositionList pl = { 0 };
            Position positions[64];
            pl.positions = positions;

            (void)bitboard_to_position_list(gs->side[side].piece[p], &pl);

            for (int i = 0; i < pl.length; i++) {
                Position pos = side == S_WHITE
                    ? pl.positions[i]
                    : invert_board(pl.positions[i]);

                assert(position_is_legal(pos));

                switch (p) {
                    case P_PAWN: {
                        eval += sign * PSV_PAWN[pos];
                        break;
                    }
                    case P_KNIGHT: {
                        eval += sign * PSV_KNIGHT[pos];
                        break;
                    }
                    case P_BISHOP: {
                        eval += sign * PSV_CENTRALISATION[pos];
                        break;
                    }
                    case P_ROOK: {
                        eval += sign * lerp(PSV_ROOK_OPENING[pos], PSV_CENTRALISATION[pos],
                            1.0f - pieces_on_board);

                        break;
                    }
                    case P_QUEEN: {
                        eval += sign * lerp(PSV_ROOK_OPENING[pos], PSV_CENTRALISATION[pos],
                            1.0f - pieces_on_board);

                        break;
                    }
                    case P_KING: {
                        eval += sign * lerp(PSV_KING_OPENING[pos], PSV_CENTRALISATION[pos],
                            1.0f - pieces_on_board);

                        break;
                    }
                    default: assert(false);
                }
            }
        }
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

float bool_to_float(bool b) {
    // The implicit cast was causing issues sometimes.
    return b ? 1.0f : 0.0f;
}

float evaluate_king_safety(GameState* gs);

float evaluate(GameState* gs) {
    switch (gs->game_result) {
        case R_WHITE_WON: return INFINITY;
        case R_BLACK_WON: return -INFINITY;
        case R_DRAW: return 0;
        default: break;
    }

    float checks = -bool_to_float(gs->side[S_WHITE].in_check)
        + bool_to_float(gs->side[S_BLACK].in_check);

    float white_castle_rights = bool_to_float(gs->side[S_WHITE].lost_k_castle_rights)
        + bool_to_float(gs->side[S_WHITE].lost_q_castle_rights);

    float black_castle_rights = bool_to_float(gs->side[S_BLACK].lost_k_castle_rights)
        + bool_to_float(gs->side[S_BLACK].lost_q_castle_rights);

    float castle_rights = -white_castle_rights + black_castle_rights;

    float absolute_pins = set_bits_count(gs->side[S_BLACK].pinned_pieces)
        - set_bits_count(gs->side[S_WHITE].pinned_pieces);

    #ifdef DEBUG_EVAL
        printf("------ Eval breakdown ------\n");
        printf("material: %f\n", evaluate_material(gs));
        printf("castle_rights: %f\n", castle_rights);
        printf("checks: %f\n", checks);
        printf("absolute_pins: %f\n", absolute_pins);
        printf("pawn_structure: %f\n", evaluate_pawn_structure(gs));
        printf("center_control: %f\n", evaluate_center_control(gs));
        printf("king_safety: %f\n", evaluate_king_safety(gs));
        printf("piece_square_tables: %f\n", evaluate_piece_square_tables(gs));
        printf("----------------------------\n");
    #endif

    float total_eval = evaluate_material(gs) * 1.5
        + castle_rights * 0.4
        + checks * 1.4
        + absolute_pins * 0.9
        + evaluate_pawn_structure(gs) * 0.4
        + evaluate_center_control(gs) * 1.2
        + evaluate_king_safety(gs) * 0.6
        + evaluate_piece_square_tables(gs) * 1.05;

    assert(!isnan(total_eval));
    // This is an arbitrary value but it should never be this high.
    assert(!(!isinf(total_eval) && fabs(total_eval) > 1000000));

    return total_eval;
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

    update_game_state_info(&gs);

    return gs;
}

void update_game_state_info(GameState* gs) {
    update_all_piece_stores(gs);

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

    (void)get_pins(gs, S_WHITE);
    (void)get_pins(gs, S_BLACK);

    MoveList ml = { 0 };
    Move moves[128];
    ml.moves = moves;

    // Turn needs to be inverted; we are seeing if the opponent
    // will have any legal moves on the next move.
    gs->turn = !gs->turn;
    get_all_legal_moves(gs, &ml);
    gs->turn = !gs->turn;

    if (ml.length == 0) {
        if (gs->side[!gs->turn].in_check) {
            // Checkmate
            gs->game_result = (!gs->turn) == S_WHITE ? R_BLACK_WON : R_WHITE_WON;
        } else {
            // Stalemate
            gs->game_result = R_DRAW;
        }
    }
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

    update_game_state_info(gs);

    gs->turn = !gs->turn;
}

void generate_cols() {
    for (int i = 0; i < 8; i++) {
        COLS[i] = 0x0101010101010101 << i;
    }
}

void generate_rows() {
    for (int i = 0; i < 8; i++) {
        ROWS[i] = 0xFFUL << ((u64)i * 8UL);
    }
}

void occupancy_permutations(Board mask, Board* buf, int* buf_len, int buf_capacity) {
    PositionList pl = { 0 };
    Position positions[64];
    pl.positions = positions;

    bitboard_to_position_list(mask, &pl);

    int permutations = 1 << pl.length;

    assert(buf_capacity >= permutations);

    *buf_len = permutations;

    for (u64 perm = 0; perm < permutations; perm++) {
        Board b = 0;
        for (int pos_index = 0; pos_index < pl.length; pos_index++) {
            if ((perm & (1UL << pos_index)) != 0) {
                b |= pos_to_bitboard(pl.positions[pos_index]);
            }
        }
        buf[perm] = b;
    }
}

void generate_magic_bitboards(Board* magic_boards, int hash_space, u64* magic_numbers, Board* moves_lut,
    PosOffset* offsets, int offsets_len, int shift) {

    for (Position pos = 0; pos < 64; pos++) {
        #define MAX_PERMS 16384
        Board permutations[MAX_PERMS] = { 0 };
        int permutations_len = 0;

        Board mask = moves_lut[pos];

        occupancy_permutations(mask, permutations,
            &permutations_len, MAX_PERMS);

        Board attacks[MAX_PERMS] = { 0 };

        for (int i = 0; i < MAX_PERMS; i++) {
            Board occupancies = permutations[i];
            Board attacked_squares = sliding_piece_attacks(pos, occupancies, offsets, offsets_len);
            attacks[i] = attacked_squares;
        }

        retry_with_new_magic:

        for (int i = 0; i < hash_space; i++) {
            magic_boards[pos * hash_space + i] = 0;
        }

        for (int perm = 0; perm < permutations_len; perm++) {
            Board occupancies = permutations[perm];
            Board attacked_squares = attacks[perm];

            u64 magic = magic_numbers[pos];
            u64 hash = magic_board_hash(occupancies, mask, magic, shift);
            assert(hash < hash_space);

            Board current_value = magic_boards[pos * hash_space + hash];

            if (current_value != 0 && current_value != attacked_squares) {
                #ifndef GENERATE_MAGICS
                    printf("Pos: %d has an insufficient magic number (%lu).\n", pos, magic);
                    assert(false && "Hash collision!.");
                #else
                    magic_numbers[pos] = rand();
                    magic_numbers[pos] <<= 32;
                    magic_numbers[pos] |= rand();

                    goto retry_with_new_magic;
                #endif
            }

            magic_boards[pos * hash_space + hash] = attacked_squares;
        }
        #ifdef GENERATE_MAGICS
            printf("%lu,\n", magic_numbers[pos]);
        #endif
    }
}

void generate_LUTs() {
    (void)generate_knight_LUT();
    (void)generate_king_LUT();

    (void)generate_cols();
    (void)generate_rows();

    (void)generate_rook_LUT();
    (void)generate_bishop_LUT();

    (void)generate_magic_bitboards((Board*)ROOK_MAGIC_BOARDS, ROOK_PERMS, ROOK_MAGIC_NUMBERS,
        ROOK_MOVES_LUT, ROOK_OFFSETS, 4, ROOK_SHIFT);

    (void)generate_magic_bitboards((Board*)BISHOP_MAGIC_BOARDS, BISHOP_PERMS, BISHOP_MAGIC_NUMBERS,
        BISHOP_MOVES_LUT, (PosOffset*)BISHOP_OFFSETS, 4, BISHOP_SHIFT);
    // (void)generate_magic_bishop();
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
    EvaluatedMove em = minimax(5, gs, -INFINITY, INFINITY, tt);

    if (is_null_move(&em.move)) {
        // printf("%s has forced mate!\n", em.eval > 0 ? "White" : "Black");

        MoveList ml = { 0 };
        Move moves[128];
        ml.moves = moves;
        get_all_legal_moves(gs, &ml);
        if (ml.length == 0) {
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

        if (is_null_move(&em.move)) {
            printf("Null move!");
            break;
        }

        printf("Playing move: %s\n", move_notation);
        printf("Eval: %f\n", em.eval);

        PositionList pl = { 0 };
        Position positions[2] = { em.move.data.from, em.move.data.to };
        pl.positions = positions;
        pl.length = 2;

        print_game_state(gs, &pl);

        printf("PSV: %f\n", evaluate_piece_square_tables(gs));

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
            (void)update_game_state_info(gs);
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
                (void)update_game_state_info(gs);
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
            print_game_state(gs, NULL);
            printf("%s to move\n", gs->turn == S_WHITE ? "white" : "black");
        } else {
            printf("Unknown command: %s\n", line);
        }
        fflush(stdout);
    }
}

int main(int argc, char* argv[]) {
    srand(0xbadd);

    GameState gs = { 0 };

    (void)generate_LUTs();

    printf("Initialised.\n");

    (void)set_up_board(&gs);

    (void)update_all_piece_stores(&gs);

    TranspositionTable tt = new_tt(1e8);
    // printf("TT capacity: %f GiB\n", (float)(tt.capacity * sizeof(TranspositionTableEntry)) / (float)(1024 * 1024 * 1024));

    #define move(m) apply_move(&gs, notation_to_move(m, &gs))

    // GameState gs_ = parse_FEN("2r1nrk1/p2q1ppp/bp1p4/n1pPp3/P1P1P3/2PBB1N1/4QPPP/R4RK1 w - - bm f4");
    // update_all_piece_stores(&gs_);

    // play_self_loop(&gs, &tt);
    UCI_loop(&gs, &tt);

    // FIXME: position startpos moves g2g3 c7c6 f1g2 d7d5 g1f3 h7h6 e1g1 h8h7 d2d4 c8f5 b1c3 f7f6 c1f4 g7g5 f4e3 h7g7 d1d2 d8b6 a1d1 a7a6 b2b3 h6h5 h2h4 g5h4 g3h4 f5h3 e3g5 h3g2 f1e1 g2f3 g1f1 f3e4 g5f4 e4g2 f1g1 g2e4 g1h2 b6d8 f4h6 g8h6 d2h6 d8d6 h6f4 a6a5 e2e3 e4c2 d1d2 g7g4 f4d6
    // This is a abs pinned piece taking the piece pinning it.

    // FIXME: position startpos moves e2e4 e7e6 d2d4 g7g6 g1f3 b8c6 b1c3 f8b4 f1b5 a7a6 b5c6 b7c6 c1d2 h7h6 e1g1 b4a5 d2f4 h8h7 d4d5 a5c3 b2c3 e8f8 d1d4 g6g5 f4e5 d7d6 d5c6 g5g4 f3h4 d6e5 d4e5 d8h4 e5c5 h4e7 c5b4 f8g7 a1e1 e7b4 c3b4 a8b8 c2c3 a6a5 a2a3 c8a6 c3c4 a6c4 e1e3 c4f1 g1f1 b8b4 e3g3 b4e4 f2f3 g3f3 e4e5 f3f4
    // 2023-10-15 21:32:35.094-->1:go infinite
    // 2023-10-15 21:32:35.096<--1:engine: main.c:394: void assert_piece_exists(Piece, Position, GameState *): Assertion `gs->side[gs->turn].piece[p] & pos_to_bitboard(pos)' failed.

    // FIXME: position startpos moves g2g3 c7c6 f1g2 d7d5 g1f3 h7h6 e1g1 h8h7 d2d4 c8f5 c1f4 g7g5 f3h4 g5f4 h4f5 e7e6 b1c3 e6f5 d1d3 d8d6 d3f5 d6d8 f5h7 d8c7 h7g8 e8e7 g8h7 b7b5 e2e4 b5b4 c3e2 d5e4 e2f4 f8g7 h7g7 b8a6 g7h6 a8g8 h6h7 c7b8 a1e1 b8b6 e1e4 e7d8 h7g8 d8c7 g8f7

    return 0;
}
