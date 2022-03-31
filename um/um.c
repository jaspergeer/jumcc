/**
 * @file um.c
 * @author Jimmy Maslen and Jasper Geer
 * @brief The implementation of a um emulator. The emulator executes a series
 *        of bitpacked instructions passed to the program as a file.
 *        
 * @version 0.1
 * @date 2021-11-17
 * 
 * @copyright Copyright (c) 2021
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/stat.h>
#include <assert.h>

typedef enum Um_opcode
{
    CMOV = 0,
    SLOAD,
    SSTORE,
    ADD,
    MUL,
    DIV,
    NAND,
    HALT,
    ACTIVATE,
    INACTIVATE,
    OUT,
    IN,
    LOADP,
    LV
} Um_opcode;

const unsigned NUM_REGISTERS = 8;
const unsigned BYTES_PER_WORD = 4;
const unsigned INIT_MEMORY_SIZE = 16384;
const unsigned INIT_UNUSED_SIZE = 1024;

const uint32_t RA_MASK = 0x1c0;
const uint32_t RB_MASK = 0x038;
const uint32_t RC_MASK = 0x007;
const uint32_t LV_R_MASK = 0xe000000;
const uint32_t LV_VAL_MASK = 0x01ffffff;

FILE *open_file(char *filename);

/**
 * @brief Checks that the appropriate number of arguments are passed in,
 *        and that the file exists. Reads the file that is passed and 
 *        initializes the memory array. Executes the program.
 * 
 * @param argc - number of command line arguments
 * @param argv - array of command line arguments
 * @return int - exit code
 */
int main(int argc, char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: um filename.um\n");
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");
    assert(file != NULL);

    uint32_t registers[NUM_REGISTERS];
    for (unsigned i = 0; i < NUM_REGISTERS; i++) {
        registers[i] = 0;
    }
    
    int size = INIT_MEMORY_SIZE + 1;
    int next_identifier = 1;
    uint32_t **segments = calloc(size, sizeof(uint32_t *));
    uint32_t unused_size = INIT_UNUSED_SIZE;
    int next_unused_index = -1;
    uint32_t *unused = calloc(unused_size, sizeof(uint32_t));

    struct stat st;
    stat(argv[1], &st);
    size_t file_size = st.st_size;
    
    segments[0] = calloc(sizeof(uint32_t), (file_size / BYTES_PER_WORD) + 1);

    for (size_t i = 0; i < file_size; i += BYTES_PER_WORD) {
        uint32_t word = 0;
        uint8_t this_byte = 0;
        fread(&this_byte, 1, 1, file);
        word = word | (this_byte << 24);
        fread(&this_byte, 1, 1, file);
        word = word | (this_byte << 16);
        fread(&this_byte, 1, 1, file);
        word = word | (this_byte << 8);
        fread(&this_byte, 1, 1, file);
        word = word | (this_byte << 0);
        
        *(segments[0] + (i / BYTES_PER_WORD) + 1) = word;
    }
    
    uint32_t *program_counter = segments[0] + 1;
    int is_active = 1;
    while (is_active) {
        uint32_t word = *(program_counter);
        uint32_t opcode = word >> 28;
        program_counter++;

        if (opcode != LV) {
            uint32_t ra = (word & RA_MASK) >> 6;
            uint32_t rb = (word & RB_MASK) >> 3;
            uint32_t rc = word & RC_MASK;
            switch (opcode)
            {
            case CMOV:
                if (registers[rc] != 0) {
                    registers[ra] = registers[rb];
                }
                break;
            case SLOAD: ;
                registers[ra] = *(segments[registers[rb]] + registers[rc] + 1);
                break;
            case SSTORE:
                *(segments[registers[ra]] + registers[rb] + 1) = registers[rc];
                break;
            case ADD:
                registers[ra] = registers[rb] + registers[rc];
                break;
            case MUL:
                registers[ra] = registers[rb] * registers[rc];
                break;
            case DIV:
                registers[ra] = registers[rb] / registers[rc];
                break;
            case NAND:
                registers[ra] = ~(registers[rb] & registers[rc]);
                break;
            case HALT:
                is_active = 0;
                break;
            case ACTIVATE: ;
                uint32_t *new_seg = calloc(sizeof(uint32_t), registers[rc] + 1);
                new_seg[0] = registers[rc];
                uint32_t index;
                if (next_unused_index == -1) {
                    index = next_identifier++;
                    if (index >= size) {
                        uint32_t **new_memory = calloc(size * 2 + 1, sizeof(uint32_t *));
                        
                        for (int i = 0; i < size; i++) {
                            new_memory[i] = segments[i];
                        }
                        size = size * 2 + 1;
                        free(segments);
                        segments = new_memory;
                    }
                } else {
                    index = unused[next_unused_index];
                    next_unused_index--;
                }
                segments[index] = new_seg;
                registers[rb] = index;
                break;
            case INACTIVATE: ;
                uint32_t *seg = segments[registers[rc]];
                free(seg);
                next_unused_index++;
                if (next_unused_index == unused_size) {
                    uint32_t *new_unused = calloc(unused_size * 2 + 1, sizeof(uint32_t));
                    for (uint32_t i = 0; i < unused_size; i++) {
                        new_unused[i] = unused[i];
                    }
                    unused_size = unused_size * 2 + 1;
                    free(unused);
                    unused = new_unused;
                }
                unused[next_unused_index] = registers[rc];
                segments[registers[rc]] = NULL;
                break;
            case OUT:
                putc(registers[rc], stdout);
                break;
            case IN: ;
                int character = getchar();
                registers[rc] = (character == EOF)? 0xffffffff : 
                                (uint32_t) character;
                break;
            case LOADP: ;
                if (registers[rb] == 0) {
                    program_counter = segments[0] + 1 + registers[rc];
                    break;
                }
                uint32_t seg_size = segments[registers[rb]][0];
                free(segments[0]);
                segments[0] = calloc(sizeof(uint32_t), seg_size + 1);
                for (uint32_t i = 0; i < seg_size; i++) {
                    segments[0][i + 1] = segments[registers[rb]][i + 1];
                }
                program_counter = segments[0] + 1 + registers[rc];
                break;
            }
        } else {
            uint32_t r = (word & LV_R_MASK) >> 25;
            uint32_t value = word & LV_VAL_MASK;
            registers[r] = value;
        }
    }
    
    for (int i = 0; i < next_identifier; i++) {
        if (segments[i] != NULL) {
            free(segments[i]);
        }
    }
    free(segments);
    free(unused);

    fclose(file);
    return 0;
}