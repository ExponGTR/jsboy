var instr_count = 0;
var instr_log = false;

var dpixels = new Uint8Array(160 * 144);

var display = document.getElementById('display'),
    dctx = display.getContext('2d');

var dImgData = dctx.getImageData(0, 0, 160, 144);

for (i = 0; i < 160 * 144; i++) {
    dImgData.data[4 * i + 3] = 255;
}

dctx.putImageData(dImgData, 0, 0);

function renderDisplayCanvas() {
    var R = [224,136,52, 8 ],
        G = [248,192,104,24],
        B = [208,112,86, 32];
    for (i = 0, j = 0; i < 160 * 144; i++) {
        dImgData.data[j++] = R[dpixels[i]];
        dImgData.data[j++] = G[dpixels[i]];
        dImgData.data[j] = B[dpixels[i]];
        j += 2;
    }
    dctx.putImageData(dImgData, 0, 0);
}

const pixelDecoder = [];
for (var d1 = 0; d1 < 256; d1++) {
    pixelDecoder[d1] = [];
    for (var d2 = 0; d2 < 256; d2++) {
        pixelDecoder[d1][d2] = [
            ((d1 & 128) + 2 * (d2 & 128)) >> 7,
            ((d1 & 64) + 2 * (d2 & 64)) >> 6,
            ((d1 & 32) + 2 * (d2 & 32)) >> 5,
            ((d1 & 16) + 2 * (d2 & 16)) >> 4,
            ((d1 & 8) + 2 * (d2 & 8)) >> 3,
            ((d1 & 4) + 2 * (d2 & 4)) >> 2,
            ((d1 & 2) + 2 * (d2 & 2)) >> 1,
            ((d1 & 1) + 2 * (d2 & 1))
        ];
    }
}

/*
Memory map

    $FFFF       	Interrupt Enable Flag
    $FF80-$FFFE 	HRAM - 127 bytes
    $FF00-$FF7F 	Hardware I/O Registers
    $FEA0-$FEFF 	Unusable Memory
    $FE00-$FE9F 	OAM - Object Attribute Memory
    $E000-$FDFF 	Echo RAM - Reserved, Do Not Use
    $D000-$DFFF 	Internal RAM - Bank 1-7 (switchable - CGB only)
    $C000-$CFFF 	Internal RAM - Bank 0 (fixed)
    $A000-$BFFF 	Cartridge RAM (If Available)
    $9C00-$9FFF 	BG Map Data 2
    $9800-$9BFF 	BG Map Data 1
    $8000-$97FF 	Character RAM
    $4000-$7FFF 	Cartridge ROM - Switchable Banks 1-xx
    $0150-$3FFF 	Cartridge ROM - Bank 0 (fixed)
    $0100-$014F 	Cartridge Header Area
    $0000-$00FF 	Restart and Interrupt Vectors

Bank switch:  write bank number to $2000 

    Cartridge RAM has separate bank number to cartridge ROM

Bootcode: 
    load first bank of rom into MEM
    overwrite first 256 bytes with bootcode
    after verification, overwrite first 256 bytes with data from rom
*/

var MEM = new Uint8Array(0x10000); // Main address space
var FirstROMPage, ROM = new Uint8Array(512); //populate later
var ROMbank = 1;
var ROMbankoffset = (ROMbank - 1) * 0x4000;

var cartRAM = new Uint8Array(0x8000); // some carts have up to 128K of ram?
var RAMbank = 0;
var RAMbankoffset = RAMbank * 0x2000 - 0xA000;
var RAMenabled = false;
var MBCRamMode = 0; //for MBC1

var divPrescaler = 0,
    timerPrescaler = 0,
    timerSpeed = 1,
    timerEnable = false;
var LCD_enabled = false,
    LCD_lastmode = 1,
    LCD_scan = 0;
var tileData = new Uint8Array((0x9800 - 0x8000) / 16);
var joypad_dpad = 0xEF,
    joypad_buttons = 0xDF; // 0=pressed

document.addEventListener('keydown', handleKeyDown);
document.addEventListener('keyup', handleKeyUp);
function handleKeyDown(e) {
    e.preventDefault(); // Prevent default browser behavior (e.g., scrolling)
    switch (e.key.toLowerCase()) {
        // D-pad
        case 'arrowup':    joypad_dpad &= ~(1 << 2); break; // Up
        case 'arrowdown':  joypad_dpad &= ~(1 << 3); break; // Down
        case 'arrowleft':  joypad_dpad &= ~(1 << 1); break; // Left
        case 'arrowright': joypad_dpad &= ~(1 << 0); break;  // Right
        // Buttons
        case 'z':          joypad_buttons &= ~(1 << 0); break; // A
        case 'x':          joypad_buttons &= ~(1 << 1); break; // B
        case 'enter':      joypad_buttons &= ~(1 << 3); break; // Start
        case 'shift':      joypad_buttons &= ~(1 << 2); break; // Select
    }
}
  
  function handleKeyUp(e) {
    e.preventDefault();
    switch (e.key.toLowerCase()) {
      // D-pad
      case 'arrowup':    joypad_dpad |= (1 << 2); break; // Up
      case 'arrowdown':  joypad_dpad |= (1 << 3); break; // Down
      case 'arrowleft':  joypad_dpad |= (1 << 1); break; // Left
      case 'arrowright': joypad_dpad |= (1 << 0); break; // Right
      
      // Buttons
      case 'z':          joypad_buttons |= (1 << 0); break; // A
      case 'x':          joypad_buttons |= (1 << 1); break; // B
      case 'enter':      joypad_buttons |= (1 << 3); break; // Start
      case 'shift':      joypad_buttons |= (1 << 2); break; // Select
    }
}

// Reference: https://gbdev.io/pandocs/Memory_Map.html

function readMem(addr) {
    if (addr <= 0x3fff) return ROM[addr];
    if (addr <= 0x7fff) return ROM[addr + ROMbankoffset];

    // Cartridge RAM
    if (addr >= 0xA000 && addr <= 0xBFFF) return cartRAM[addr + RAMbankoffset];

    // Joypad
    if (addr === 0xFF00) {
        if (MEM[0xFF00] & 0x20) {
            return 0xE0 | joypad_dpad;
        } else if (MEM[0xFF00] & 0x10) {
            return 0xD0 | joypad_buttons;
        } else return 0;
    }

    return MEM[addr];
}

function readMem16(addr) {
    return [readMem(addr + 1), readMem(addr)];
}

function writeMem(addr, data) {
    if (addr <= 0x7fff) {
        doMBC(addr, data);
        return;
    }

    if (addr >= 0xA000 && addr <= 0xBFFF && RAMenabled) {
        cartRAM[addr + RAMbankoffset] = data;
        return;
    }

    //DIV register: reset
    if (addr === 0xFF04) {
        MEM[0xFF04] = 0;
        return;
    }
    // Timer control
    if (addr === 0xFF07) {
        timerEnable = (data & (1 << 2) != 0);
        timerSpeed = [1024, 64, 16, 4][data & 0x3];
        MEM[addr] = data;
        return;
    }

    //LCD control
    if (addr === 0xFF40) {
        var cc = data & (1 << 7);
        if (LCD_enabled != cc) {
            LCD_enabled = !!cc;
            if (!LCD_enabled) { // Disabling the display sets it to mode 1
                // this should also probably set all pixels to white
                LCD_scan = 0;
                MEM[0xFF41] = (MEM[0xFF41] & 0xFC) + 1;
            }
        }
    }
    if (addr === 0xFF41) {
        //don't overwrite the lowest two bits (mode)
        MEM[0xFF41] &= 0x3;
        data &= 0xFC;
        MEM[0xFF41] |= (0x80 | data);
        return;
    }

    // LY - write causes reset
    if (addr === 0xFF44) {
        MEM[0xFF44] = 0;
        return;
    }
      // FF46 - DMA - DMA Transfer and Start Address (W)
    if (addr == 0xFF46) {
        var st = data << 8;
        for (var i = 0; i <= 0x9F; i++) 
        MEM[0xFE00 + i] = readMem(st + i);
        return
    }


    // disable bootrom
    if (addr === 0xFF50) {
        for (var i = 0; i < 256; i++) ROM[i] = FirstROMPage[i];
        return;
    }

    MEM[addr] = data;
}

function writeMem16(addr, dataH, dataL) {
    writeMem(addr, dataL);
    writeMem(addr + 1, dataH);
}

function doMBC(addr, data) {
    switch (ROM[0x147]) {
        // Cartridge Type = ROM[0x147]

        case 0: // ROM ONLY
            break;

        case 1: // MBC1
        case 2: // MBC1+RAM
        case 3: // MBC1+RAM+BATTERY
            if (addr <= 0x1FFF)
                RAMenabled = ((data & 0x0F) === 0xA);
            else if (addr <= 0x3FFF) {
                data &= 0x1F;
                if (data === 0) data = 1; // MBC1 translates bank 0 to bank 1 (apparently regardless of upper bits)
                // set lowest 5 bits of bank number
                ROMbank = (ROMbank & 0xE0) | (data & 0x1F);
                ROMbankoffset = (ROMbank - 1) * 0x4000  % ROM.length;
            } else if (addr <= 0x5fff) {
                data &= 0x3;
                if (MBCRamMode === 0) {
                    ROMbank = (ROMbank & 0x1F) | (data << 5);
                    ROMbankoffset = (ROMbank - 1) * 0x4000  % ROM.length;
                } else {
                    RAMbank = data;
                    RAMbankoffset = RAMbank * 0x2000 - 0xA000;
                }
            } else {
                MBCRamMode = data & 1;
                if (MBCRamMode === 0) {
                    RAMbank = 0;
                    RAMbankoffset = RAMbank * 0x2000 - 0xA000;
                } else {
                    ROMbank &= 0x1F;
                    ROMbankoffset = (ROMbank - 1) * 0x4000 % ROM.length;
                }
            }
            break;
        // case 0x05: //  MBC2
        // case 0x06: //  MBC2+BATTERY
        
        // case 0x08: //  ROM+RAM
        // case 0x09: //  ROM+RAM+BATTERY
        // case 0x0B: //  MMM01
        // case 0x0C: //  MMM01+RAM
        // case 0x0D: //  MMM01+RAM+BATTERY
        // case 0x0F: //  MBC3+TIMER+BATTERY
        // case 0x10: //  MBC3+TIMER+RAM+BATTERY
        case 0x11: //  MBC3
        case 0x12: //  MBC3+RAM
        case 0x13: //  MBC3+RAM+BATTERY
        
            if (addr <= 0x1FFF) {
            RAMenabled = ((data & 0x0F) == 0xA) 
            } else if (addr <= 0x3FFF){
            if (data === 0) data = 1 // allows access to banks 0x20, 0x40, 0x60
            ROMbank = data & 0x7F;
            ROMbankoffset = (ROMbank-1)*0x4000 %ROM.length;
            } else if (addr <= 0x5fff) {
            if (data < 8) {
                RAMbank = data;
                RAMbankoffset = RAMbank*0x2000 - 0xA000
            } else{
                // RTC registers here
            }
            } else {
            // RTC latch
            }
        break;
        
        // case 0x19: //  MBC5
        // case 0x1A: //  MBC5+RAM
        // case 0x1B: //  MBC5+RAM+BATTERY
        // case 0x1C: //  MBC5+RUMBLE
        // case 0x1D: //  MBC5+RUMBLE+RAM
        // case 0x1E: //  MBC5+RUMBLE+RAM+BATTERY
        // case 0x20: //  MBC6
        // case 0x22: //  MBC7+SENSOR+RUMBLE+RAM+BATTERY
        // case 0xFC: //  POCKET CAMERA
        // case 0xFD: //  BANDAI TAMA5
        // case 0xFE: //  HuC3
        // case 0xFF: //  HuC1+RAM+BATTERY
        default:
            throw Error("Unimplemented memory controller");
    }
}

var REG = new Uint8Array(8);

var FLAGS = {
  Z: false,
  N: false,
  H: false,
  C: false
};

var PC = 0;
var SP = 0;
var IME = false; // Interrupt master enable

var cpu_halted = false;

const A = 0b111;
const B = 0b000;
const C = 0b001;
const D = 0b010;
const E = 0b011;
const H = 0b100;
const L = 0b101;
const HL = 0b110;

const Immediate = 257;
const BC = 258;
const DE = 259;
const SPr = 260;

const opcodes = Array(256);
for (var i = 0; i < 256; i++) opcodes[i] = function() {
  throw Error("Undefined Opcode");
};

const CBcodes = Array(256);
for (var i = 0; i < 256; i++) CBcodes[i] = function() {
  throw Error("Undefined 0xCB Opcode");
};



function LD(a, b) {
  if (b === Immediate) return () => {
      REG[a] = readMem(PC + 1);
      PC += 2;
      return 8;
  }

  return () => {
      REG[a] = REG[b];
      PC++;
      return 4;
  }
}

function LD_FROM_MEM_PTR(a, b, c) {
  if (b === Immediate) return () => {
      REG[a] = readMem(readMem(PC + 1) + (readMem(PC + 2) << 8));
      PC += 3;
      return 12;
  }

  return () => {
      REG[a] = readMem((REG[b] << 8) + REG[c]);
      PC++;
      return 8;
  }
}

function LD_TO_MEM_PTR(a, b, c) {
  if (a === Immediate) return () => {
      writeMem(readMem(PC + 1) + (readMem(PC + 2) << 8), REG[b]);
      PC += 3;
      return 16;
  }
  if (c === Immediate) return () => {
      writeMem((REG[a] << 8) + REG[b], readMem(PC + 1));
      PC += 2;
      return 12;
  }
  return () => {
      writeMem((REG[a] << 8) + REG[b], REG[c]);
      PC++;
      return 8;
  }
}

function LD16(a, b, c) {
  if (b === Immediate) {
      if (a === HL) return () => {
          // mem to hl
          var s = readMem16(readMem(PC + 1) + (readMem(PC + 2) << 8));

          REG[H] = s[0];
          REG[L] = s[1];

          PC += 3;
          return 12;
      }

      // immediate into SP... 
      return () => {
          SP = readMem(PC + 1) + (readMem(PC + 2) << 8);
          PC += 3;
          return 12;
      }
  }
  if (c === Immediate) return () => {
      REG[a] = readMem(PC + 2);
      REG[b] = readMem(PC + 1);

      PC += 3;
      return 12;
  }

  // ld sp, hl
  return () => {
      SP = (REG[H] << 8) + REG[L];
      PC++;
      return 8;
  }
}

function LDD(a, b) {
  if (a === HL) return () => {
      writeMem((REG[H] << 8) + REG[L], REG[A]);

      if (REG[L] === 0) REG[H]--;
      REG[L]--;
      
      PC++;
      return 8;
  }

  return () => {
      REG[A] = readMem((REG[H] << 8) + REG[L]);

      if (REG[L] === 0) REG[H]--;
      REG[L]--;

      PC++;
      return 8;
  }
}

function LDI(a, b) {
  if (a === HL) return () => {
      writeMem((REG[H] << 8) + REG[L], REG[A]);
      
      if (REG[L] === 255) REG[H]++;
      REG[L]++;
      
      PC++;
      return 8;
  }

  return () => {
      REG[A] = readMem((REG[H] << 8) + REG[L]);

      if (REG[L] === 255) REG[H]++;
      REG[L]++;

      PC++;
      return 8;
  }
}

function LDC(a, b) {
  if (a === A) return () => { //LD   A, (FF00+C)
      REG[A] = readMem(0xFF00 + REG[C]);
      PC++;
      return 8;
  }
  return () => { //LD   (FF00+C),A
      writeMem(0xFF00 + REG[C], REG[A]);
      PC++;
      return 8;
  }
}

function LDH(a, b) {
  if (a === A) return () => { //LD   A, (FF00+n)
      REG[A] = readMem(0xFF00 + readMem(PC + 1));
      PC += 2;
      return 12;
  }
  return () => { //LD   (FF00+n),A
      writeMem(0xFF00 + readMem(PC + 1), REG[A]);
      PC += 2;
      return 12;
  }
}

function ALU(op, a, b) {
  if (b === Immediate) return () => {
      REG[A] = ALU_operation(op, readMem(PC + 1));
      PC += 2;
      return 8;
  }
  if (b === HL) return () => {
      REG[A] = ALU_operation(op, readMem((REG[H] << 8) + REG[L]));
      PC++;
      return 8;
  }
  return () => {
      REG[A] = ALU_operation(op, REG[b]);
      PC++;
      return 4;
  }
}

const ADD = 1;
const ADC = 2;
const SUB = 3;
const SBC = 4;
const AND = 5;
const OR  = 6;
const XOR = 7;
const CP  = 8;

function ALU_operation(op, b) {
  var result = REG[A];
  FLAGS.N = false;

  switch (op) {
      case ADD: 
          FLAGS.H = !!(((REG[A] & 0x0F) + (b & 0x0F)) & 0x10);
          result += b;
          break;
      case ADC:
          FLAGS.H = !!(((REG[A] & 0x0F) + (b & 0x0F) + FLAGS.C) & 0x10);
          result += b + FLAGS.C;
          break;
      case SUB: 
          result -= b;
          FLAGS.N = true;
          FLAGS.H = !!(((REG[A] & 0x0F) - (b & 0x0F)) & 0x10);
          break;
      case CP:
          result -= b;
          FLAGS.N = true;
          FLAGS.H = !!(((REG[A] & 0x0F) - (b & 0x0F)) & 0x10);
          FLAGS.Z = ((result & 0xff) === 0);
          FLAGS.C = result > 255 || result < 0;
          return REG[A];
      case SBC:
          result -= b + FLAGS.C;
          FLAGS.N = true;
          FLAGS.H = !!(((REG[A] & 0x0F) - (b & 0x0F) - FLAGS.C) & 0x10);
          break;
      case AND:
          result &= b;
          FLAGS.H = true;
          break;
      case OR:
          result |= b;
          FLAGS.H = false;
          break;
      case XOR: 
          result ^= b;
          FLAGS.H = false;
          break;
  }

  FLAGS.Z = ((result & 0xff) === 0);
  FLAGS.C = result > 255 || result < 0;

  return result & 0xFF;
}

function INC(a) { return incdec(a, 1); }
function DEC(a) { return incdec(a, -1); }

function incdec(r, offset) {
  if (r === HL) return () => {
      writeMem((REG[H] << 8) + REG[L], incdec_process(readMem((REG[H] << 8) + REG[L]), offset));
      PC++;
      return 12;
  }

  return () => {
      REG[r] = incdec_process(REG[r], offset);
      PC++;
      return 4;
  }
}

function incdec_process(a, offset) {
  var result = a + offset;
  FLAGS.H = (a & (1 << 4)) != (result & (1 << 4));
  FLAGS.N = offset === -1;
  FLAGS.Z = ((result & 0xff) === 0);
  return result;
}

// 16 bit inc / dec affect no flags
function INC16(a, b) {
  if (a === SPr) return () => {
      SP++;
      PC++;
      return 8;
  }
  return () => {
      if (REG[b] === 255) REG[a]++;
      REG[b]++;
      PC++;
      return 8;
  }
}

function DEC16(a, b) {
  if (a === SPr) return () => {
      SP--;
      PC++;
      return 8;
  }
  return () => {
      if (REG[b] === 0) REG[a]--;
      REG[b]--;
      PC++;
      return 8;
  }
}

function signedOffset(b) {
  return (b > 127) ? (b - 256) : b;
}

function JRNZ() {
  if (FLAGS.Z) { PC += 2; return 8; }
  PC += 2 + signedOffset(readMem(PC + 1));
  return 12;
}

function JRNC() {
  if (FLAGS.C) { PC += 2; return 8; }
  PC += 2 + signedOffset(readMem(PC + 1));
  return 12;
}

function JRZ() {
  if (!FLAGS.Z) { PC += 2; return 8; }
  PC += 2 + signedOffset(readMem(PC + 1));
  return 12;
}

function JRC() {
  if (!FLAGS.C) { PC += 2; return 8; }
  PC += 2 + signedOffset(readMem(PC + 1));
  return 12;
}

function JR() {
  PC += 2 + signedOffset(readMem(PC + 1));
  return 12;
}

function JP() {
  PC = readMem(PC + 1) + (readMem(PC + 2) << 8);
  return 16;
}

function JPNZ() {
  if (FLAGS.Z) { PC += 3; return 12; }
  PC = readMem(PC + 1) + (readMem(PC + 2) << 8);
  return 16;
}

function JPNC() {
  if (FLAGS.C) { PC += 3; return 12; }
  PC = readMem(PC + 1) + (readMem(PC + 2) << 8);
  return 16;
}

function JPZ() {
  if (!FLAGS.Z) { PC += 3; return 12; }
  PC = readMem(PC + 1) + (readMem(PC + 2) << 8);
  return 16;
}

function JPC() {
  if (!FLAGS.C) { PC += 3; return 12; }
  PC = readMem(PC + 1) + (readMem(PC + 2) << 8);
  return 16;
}

function JPHL() {
  PC = (REG[H] << 8) + REG[L];
  return 4;
}

function PUSH(a, b) {
  if (a === A) return () => {
      var flags = (FLAGS.Z << 7) + (FLAGS.N << 6) + (FLAGS.H << 5) + (FLAGS.C << 4)
      SP -= 2
      writeMem16(SP, REG[A], flags)
      PC++
      return 16
  }
  return () => {
      SP -= 2
      writeMem16(SP, REG[a], REG[b])
      PC++
      return 16
  }
}

function POP(a, b) {
  if (a === A) return () => {
      var s = readMem16(SP);
      REG[A] = s[0]
      FLAGS.Z = (s[1] & (1 << 7)) != 0
      FLAGS.N = (s[1] & (1 << 6)) != 0
      FLAGS.H = (s[1] & (1 << 5)) != 0
      FLAGS.C = (s[1] & (1 << 4)) != 0
      SP += 2
      PC++
      return 12
  }
  return () => {
      var s = readMem16(SP);
      REG[a] = s[0]
      REG[b] = s[1]
      SP += 2
      PC++
      return 12
  }
}

function CALL() {
  SP -= 2
  var npc = PC + 3
  writeMem16(SP, npc >> 8, npc & 0xFF)
  PC = readMem(PC + 1) + (readMem(PC + 2) << 8)
  return 24
}

function CALLNZ() {
  if (FLAGS.Z) { PC += 3; return 12 }
  return CALL()
}

function CALLNC() {
  if (FLAGS.C) { PC += 3; return 12 }
  return CALL()
}

function CALLZ() {
  if (!FLAGS.Z) { PC += 3; return 12 }
  return CALL()
}

function CALLC() {
  if (!FLAGS.C) { PC += 3; return 12 }
  return CALL()
}

function RET() {
  var s = readMem16(SP);
  SP += 2
  PC = (s[0] << 8) + s[1]
  return 16
}

function RETNZ() {
  if (FLAGS.Z) { PC++; return 8 }
  RET()
  return 20
}

function RETNC() {
  if (FLAGS.C) { PC++; return 8 }
  RET()
  return 20
}

function RETZ() {
  if (!FLAGS.Z) { PC++; return 8 }
  RET()
  return 20
}

function RETC() {
  if (!FLAGS.C) { PC++; return 8 }
  RET()
  return 20
}

function RETI() {
  IME = true
  return RET()
}

function EI() {
  IME = true
  PC++
  return 4
}

function DI() {
  IME = false
  PC++
  return 4
}

function RST(a) {
  return () => {
      SP -= 2
      var npc = PC + 1
      writeMem16(SP, npc >> 8, npc & 0xFF)
      PC = a
      return 16
  }
}

const RLC = 1
const RRC = 2
const RL = 3
const RR = 4
const SLA = 5
const SRA = 6
const SRL = 7

function SHIFT(op, a) {
  if (a === HL) return () => {
      var addr = (REG[H] << 8) + REG[L];
      writeMem(addr, shift_operation(op, readMem(addr)))
      PC++
      return 16
  }
  return () => {
      REG[a] = shift_operation(op, REG[a])
      PC++
      return 8
  }
}

function shift_operation(op, a) {
  var bit7 = a >> 7, bit0 = a & 1;

  switch (op) {
      case RLC: // Rotate byte left, save carry
          a = ((a << 1) & 0xff) + bit7
          FLAGS.C = !!bit7
          break;
      case RRC: // Rotate byte right, save carry
          a = ((a >> 1) & 0xff) + (bit0 << 7)
          FLAGS.C = !!bit0
          break;
      case RL: // Rotate left through carry
          a = ((a << 1) & 0xff) + FLAGS.C
          FLAGS.C = !!bit7
          break;
      case RR: // Rotate right through carry
          a = ((a >> 1) & 0xff) + (FLAGS.C << 7)
          FLAGS.C = !!bit0
          break;
      case SLA: // Shift left
          a = ((a << 1) & 0xff)
          FLAGS.C = !!bit7
          break;
      case SRA: // Shift right arithmetic
          a = ((a >> 1) & 0xff) + (bit7 << 7)
          FLAGS.C = !!bit0
          break;
      case SRL: // Shift right logical
          a = ((a >> 1) & 0xff)
          FLAGS.C = !!bit0
          break;
  }

  FLAGS.N = false
  FLAGS.H = false
  FLAGS.Z = a === 0
  return a
}

function CCF() {
  FLAGS.N = false
  FLAGS.H = false
  FLAGS.C = !FLAGS.C
  PC++
  return 4
}

function SCF() {
  FLAGS.N = false
  FLAGS.H = false
  FLAGS.C = true
  PC++
  return 4
}

function CPL() {
  REG[A] = ~REG[A]
  FLAGS.N = true
  FLAGS.H = true
  PC++
  return 4
}

function ADDHL(a, b) {
  if (a === SPr) return () => {
      var l = REG[L] + (SP & 0xFF);
      REG[L] = l;
      var h = REG[H] + (SP >> 8) + (l > 255 ? 1 : 0)
      FLAGS.H = (REG[H] & (1 << 4)) != (h & (1 << 4))
      REG[H] = h;
      FLAGS.C = (h > 255)
      FLAGS.N = false
      PC++
      return 8
  }
  return () => {
      var l = REG[L] + REG[b];
      REG[L] = l;
      var h = REG[H] + REG[a] + (l > 255 ? 1 : 0)
      FLAGS.H = (REG[H] & (1 << 4)) != (h & (1 << 4))
      REG[H] = h;
      FLAGS.C = (h > 255)
      FLAGS.N = false
      PC++
      return 8
  }
}

function DAA() {
  // https://github.com/mamedev/mame/blob/master/src/devices/cpu/z80/z80.cpp#L847
  var a = REG[A];
  if (FLAGS.N) {
      if (FLAGS.H) REG[A] -= 0x06;
      if (FLAGS.C) REG[A] -= 0x60;
  } else {
      if ((REG[A] & 0x0f) > 0x09 || FLAGS.H) REG[A] += 0x06;
      if (REG[A] > 0x99 || FLAGS.C) { REG[A] += 0x60; FLAGS.C = true }
  }

  FLAGS.Z = (a === 0)
  FLAGS.H = false
  PC++;
  return 4
}

function LD_IMM_SP() {
  writeMem16(readMem(PC + 1) + (readMem(PC + 2) << 8), SP >> 8, SP & 0xFF);
  PC += 2
  return 20
}

function LD_HL_SP_e() {
  var e = signedOffset(readMem(PC + 1));
  var n = SP + e;
  REG[H] = (n >> 8)
  REG[L] = n & 0xFF
  FLAGS.N = false
  FLAGS.Z = false
  FLAGS.H = Boolean(((SP & 0x0F) + (e & 0x0F)) & 0x010)
  FLAGS.C = Boolean(((SP & 0xFF) + (e & 0xFF)) & 0x100)
  PC += 2
  return 12
}

function ADD_SP_e() {
  SP += signedOffset(readMem(PC + 1))
  FLAGS.N = false
  FLAGS.Z = false
  FLAGS.H = Boolean(((SP & 0x0F) + (b & 0x0F)) & 0x010)
  FLAGS.C = Boolean(((SP & 0xFF) + (b & 0xFF)) & 0x100)
  PC += 2
  return 16
}

function HALT() {
  // if interrupts disabled, stall 1 cycle, skip next instruction and continue
    cpu_halted = true; 
    PC++;
    return 4;
}

function STOP() {
  PC += 2;
  return 4
}

function SWAP(r) {
    if (r === HL) return () => {
        var a = readMem((REG[H] << 8) + REG[L]);
        a = (a >> 4) + ((a << 4) & 0xFF);
        writeMem((REG[H] << 8) + REG[L], a);
        FLAGS.Z = (a === 0);
        FLAGS.N = false;
        FLAGS.H = false;
        FLAGS.C = false;
        PC++;
        return 16;
    }
    return () => {
        REG[r] = (REG[r] >> 4) + ((REG[r] << 4) & 0xFF);
        FLAGS.Z = (REG[r] === 0);
        FLAGS.N = false;
        FLAGS.H = false;
        FLAGS.C = false;
        PC++;
        return 8;
    }
}

function BIT(b, r) {
    b = (1 << b);

    if (r === HL) return () => {
        FLAGS.Z = ((readMem((REG[H] << 8) + REG[L]) & b) === 0);
        FLAGS.H = true;
        FLAGS.N = false;
        PC++;
        return 16;
    }
    return () => {
        FLAGS.Z = ((REG[r] & b) === 0);
        FLAGS.H = true;
        FLAGS.N = false;
        PC++;
        return 8;
    }
}

function SET(b, r) {
    b = (1 << b);

    if (r === HL) return () => {
        writeMem(
            (REG[H] << 8) + REG[L],
            readMem((REG[H] << 8) + REG[L]) | b
        );
        PC++;
        return 16;
    }
    return () => {
        REG[r] |= b;
        PC++;
        return 8;
    }
}

function RES(b, r) {
    b = ~(1 << b);

    if (r === HL) return () => {
        writeMem(
            (REG[H] << 8) + REG[L],
            readMem((REG[H] << 8) + REG[L]) & b
        );
        PC++;
        return 16;
    }
    return () => {
        REG[r] &= b;
        PC++;
        return 8;
    }
}

const UNUSED = function() { return 4 };

opcodes[0x00] = function NOP() { PC++; return 4 };
opcodes[0x01] = LD16(B, C, Immediate);
opcodes[0x02] = LD_TO_MEM_PTR(B, C, A);
opcodes[0x03] = INC16(B, C);
opcodes[0x04] = INC(B);
opcodes[0x05] = DEC(B);
opcodes[0x06] = LD(B, Immediate);
opcodes[0x07] = SHIFT(RLC, A);
opcodes[0x08] = LD_IMM_SP;
opcodes[0x09] = ADDHL(B, C);
opcodes[0x0A] = LD_FROM_MEM_PTR(A, B, C);
opcodes[0x0B] = DEC16(B, C);
opcodes[0x0C] = INC(C);
opcodes[0x0D] = DEC(C);
opcodes[0x0E] = LD(C, Immediate);
opcodes[0x0F] = SHIFT(RRC, A);

opcodes[0x10] = STOP;
opcodes[0x11] = LD16(D, E, Immediate);
opcodes[0x12] = LD_TO_MEM_PTR(D, E, A);
opcodes[0x13] = INC16(D, E);
opcodes[0x14] = INC(D);
opcodes[0x15] = DEC(D);
opcodes[0x16] = LD(D, Immediate);
opcodes[0x17] = SHIFT(RL, A);
opcodes[0x18] = JR;
opcodes[0x19] = ADDHL(D, E);
opcodes[0x1A] = LD_FROM_MEM_PTR(A, D, E);
opcodes[0x1B] = DEC16(D, E);
opcodes[0x1C] = INC(E);
opcodes[0x1D] = DEC(E);
opcodes[0x1E] = LD(E, Immediate);
opcodes[0x1F] = SHIFT(RR, A);

opcodes[0x20] = JRNZ;
opcodes[0x21] = LD16(H, L, Immediate);
opcodes[0x22] = LDI(HL, A);
opcodes[0x23] = INC16(H, L);
opcodes[0x24] = INC(H);
opcodes[0x25] = DEC(H);
opcodes[0x26] = LD(H, Immediate);
opcodes[0x27] = DAA;
opcodes[0x28] = JRZ;
opcodes[0x29] = ADDHL(H, L);
opcodes[0x2A] = LDI(A, HL);
opcodes[0x2B] = DEC16(H, L);
opcodes[0x2C] = INC(L);
opcodes[0x2D] = DEC(L);
opcodes[0x2E] = LD(L, Immediate);
opcodes[0x2F] = CPL;

opcodes[0x30] = JRNC;
opcodes[0x31] = LD16(SPr, Immediate);
opcodes[0x32] = LDD(HL, A);
opcodes[0x33] = INC16(SPr);
opcodes[0x34] = INC(HL);
opcodes[0x35] = DEC(HL);
opcodes[0x36] = LD_TO_MEM_PTR(H, L, Immediate);
opcodes[0x37] = SCF;
opcodes[0x38] = JRC;
opcodes[0x39] = ADDHL(SPr);
opcodes[0x3A] = LDD(A, HL);
opcodes[0x3B] = DEC16(SPr);
opcodes[0x3C] = INC(A);
opcodes[0x3D] = DEC(A);
opcodes[0x3E] = LD(A, Immediate);
opcodes[0x3F] = CCF;

opcodes[0x40] = LD(B, B);
opcodes[0x41] = LD(B, C);
opcodes[0x42] = LD(B, D);
opcodes[0x43] = LD(B, E);
opcodes[0x44] = LD(B, H);
opcodes[0x45] = LD(B, L);
opcodes[0x46] = LD_FROM_MEM_PTR(B, H, L);
opcodes[0x47] = LD(B, A);
opcodes[0x48] = LD(C, B);
opcodes[0x49] = LD(C, C);
opcodes[0x4A] = LD(C, D);
opcodes[0x4B] = LD(C, E);
opcodes[0x4C] = LD(C, H);
opcodes[0x4D] = LD(C, L);
opcodes[0x4E] = LD_FROM_MEM_PTR(C, H, L);
opcodes[0x4F] = LD(C, A);

opcodes[0x50] = LD(D, B);
opcodes[0x51] = LD(D, C);
opcodes[0x52] = LD(D, D);
opcodes[0x53] = LD(D, E);
opcodes[0x54] = LD(D, H);
opcodes[0x55] = LD(D, L);
opcodes[0x56] = LD_FROM_MEM_PTR(D, H, L);
opcodes[0x57] = LD(D, A);
opcodes[0x58] = LD(E, B);
opcodes[0x59] = LD(E, C);
opcodes[0x5A] = LD(E, D);
opcodes[0x5B] = LD(E, E);
opcodes[0x5C] = LD(E, H);
opcodes[0x5D] = LD(E, L);
opcodes[0x5E] = LD_FROM_MEM_PTR(E, H, L);
opcodes[0x5F] = LD(E, A);

opcodes[0x60] = LD(H, B);
opcodes[0x61] = LD(H, C);
opcodes[0x62] = LD(H, D);
opcodes[0x63] = LD(H, E);
opcodes[0x64] = LD(H, H);
opcodes[0x65] = LD(H, L);
opcodes[0x66] = LD_FROM_MEM_PTR(H, H, L);
opcodes[0x67] = LD(H, A);
opcodes[0x68] = LD(L, B);
opcodes[0x69] = LD(L, C);
opcodes[0x6A] = LD(L, D);
opcodes[0x6B] = LD(L, E);
opcodes[0x6C] = LD(L, H);
opcodes[0x6D] = LD(L, L);
opcodes[0x6E] = LD_FROM_MEM_PTR(L, H, L);
opcodes[0x6F] = LD(L, A);

opcodes[0x70] = LD_TO_MEM_PTR(H, L, B);
opcodes[0x71] = LD_TO_MEM_PTR(H, L, C);
opcodes[0x72] = LD_TO_MEM_PTR(H, L, D);
opcodes[0x73] = LD_TO_MEM_PTR(H, L, E);
opcodes[0x74] = LD_TO_MEM_PTR(H, L, H);
opcodes[0x75] = LD_TO_MEM_PTR(H, L, L);
opcodes[0x76] = HALT;
opcodes[0x77] = LD_TO_MEM_PTR(H, L, A);
opcodes[0x78] = LD(A, B);
opcodes[0x79] = LD(A, C);
opcodes[0x7A] = LD(A, D);
opcodes[0x7B] = LD(A, E);
opcodes[0x7C] = LD(A, H);
opcodes[0x7D] = LD(A, L);
opcodes[0x7E] = LD_FROM_MEM_PTR(A, H, L);
opcodes[0x7F] = LD(A, A);

opcodes[0x80] = ALU(ADD, A, B);
opcodes[0x81] = ALU(ADD, A, C);
opcodes[0x82] = ALU(ADD, A, D);
opcodes[0x83] = ALU(ADD, A, E);
opcodes[0x84] = ALU(ADD, A, H);
opcodes[0x85] = ALU(ADD, A, L);
opcodes[0x86] = ALU(ADD, A, HL);
opcodes[0x87] = ALU(ADD, A, A);
opcodes[0x88] = ALU(ADC, A, B);
opcodes[0x89] = ALU(ADC, A, C);
opcodes[0x8A] = ALU(ADC, A, D);
opcodes[0x8B] = ALU(ADC, A, E);
opcodes[0x8C] = ALU(ADC, A, H);
opcodes[0x8D] = ALU(ADC, A, L);
opcodes[0x8E] = ALU(ADC, A, HL);
opcodes[0x8F] = ALU(ADC, A, A);

opcodes[0x90] = ALU(SUB, A, B);
opcodes[0x91] = ALU(SUB, A, C);
opcodes[0x92] = ALU(SUB, A, D);
opcodes[0x93] = ALU(SUB, A, E);
opcodes[0x94] = ALU(SUB, A, H);
opcodes[0x95] = ALU(SUB, A, L);
opcodes[0x96] = ALU(SUB, A, HL);
opcodes[0x97] = ALU(SUB, A, A);
opcodes[0x98] = ALU(SBC, A, B);
opcodes[0x99] = ALU(SBC, A, C);
opcodes[0x9A] = ALU(SBC, A, D);
opcodes[0x9B] = ALU(SBC, A, E);
opcodes[0x9C] = ALU(SBC, A, H);
opcodes[0x9D] = ALU(SBC, A, L);
opcodes[0x9E] = ALU(SBC, A, HL);
opcodes[0x9F] = ALU(SBC, A, A);

opcodes[0xA0] = ALU(AND, A, B);
opcodes[0xA1] = ALU(AND, A, C);
opcodes[0xA2] = ALU(AND, A, D);
opcodes[0xA3] = ALU(AND, A, E);
opcodes[0xA4] = ALU(AND, A, H);
opcodes[0xA5] = ALU(AND, A, L);
opcodes[0xA6] = ALU(AND, A, HL);
opcodes[0xA7] = ALU(AND, A, A);
opcodes[0xA8] = ALU(XOR, A, B);
opcodes[0xA9] = ALU(XOR, A, C);
opcodes[0xAA] = ALU(XOR, A, D);
opcodes[0xAB] = ALU(XOR, A, E);
opcodes[0xAC] = ALU(XOR, A, H);
opcodes[0xAD] = ALU(XOR, A, L);
opcodes[0xAE] = ALU(XOR, A, HL);
opcodes[0xAF] = ALU(XOR, A, A);

opcodes[0xB0] = ALU(OR, A, B);
opcodes[0xB1] = ALU(OR, A, C);
opcodes[0xB2] = ALU(OR, A, D);
opcodes[0xB3] = ALU(OR, A, E);
opcodes[0xB4] = ALU(OR, A, H);
opcodes[0xB5] = ALU(OR, A, L);
opcodes[0xB6] = ALU(OR, A, HL);
opcodes[0xB7] = ALU(OR, A, A);
opcodes[0xB8] = ALU(CP, A, B);
opcodes[0xB9] = ALU(CP, A, C);
opcodes[0xBA] = ALU(CP, A, D);
opcodes[0xBB] = ALU(CP, A, E);
opcodes[0xBC] = ALU(CP, A, H);
opcodes[0xBD] = ALU(CP, A, L);
opcodes[0xBE] = ALU(CP, A, HL);
opcodes[0xBF] = ALU(CP, A, A);

opcodes[0xC0] = RETNZ;
opcodes[0xC1] = POP(B, C);
opcodes[0xC2] = JPNZ;
opcodes[0xC3] = JP;
opcodes[0xC4] = CALLNZ;
opcodes[0xC5] = PUSH(B, C);
opcodes[0xC6] = ALU(ADD, A, Immediate);
opcodes[0xC7] = RST(0x00);
opcodes[0xC8] = RETZ;
opcodes[0xC9] = RET;
opcodes[0xCA] = JPZ;
opcodes[0xCB] = () => { return CBcodes[readMem(++PC)](); };
opcodes[0xCC] = CALLZ;
opcodes[0xCD] = CALL;
opcodes[0xCE] = ALU(ADC, A, Immediate);
opcodes[0xCF] = RST(0x08);

opcodes[0xD0] = RETNC;
opcodes[0xD1] = POP(D, E);
opcodes[0xD2] = JPNC;
opcodes[0xD3] = UNUSED;
opcodes[0xD4] = CALLNC;
opcodes[0xD5] = PUSH(D, E);
opcodes[0xD6] = ALU(SUB, A, Immediate);
opcodes[0xD7] = RST(0x10);
opcodes[0xD8] = RETC;
opcodes[0xD9] = RETI; // RETI
opcodes[0xDA] = JPC;
opcodes[0xDB] = UNUSED;
opcodes[0xDC] = CALLC;
opcodes[0xDD] = UNUSED;
opcodes[0xDE] = ALU(SBC, A, Immediate);
opcodes[0xDF] = RST(0x18);

opcodes[0xE0] = LDH(Immediate, A); // LD (FF00+n),A
opcodes[0xE1] = POP(H, L);
opcodes[0xE2] = LDC(C, A); // LD (FF00+C),A
opcodes[0xE3] = UNUSED;
opcodes[0xE4] = UNUSED;
opcodes[0xE5] = PUSH(H, L);
opcodes[0xE6] = ALU(AND, A, Immediate);
opcodes[0xE7] = RST(0x20);
opcodes[0xE8] = ADD_SP_e; // ADD SP,dd
opcodes[0xE9] = JPHL;
opcodes[0xEA] = LD_TO_MEM_PTR(Immediate, A); // LD (nn),A
opcodes[0xEB] = UNUSED;
opcodes[0xEC] = UNUSED;
opcodes[0xED] = UNUSED;
opcodes[0xEE] = ALU(XOR, A, Immediate);
opcodes[0xEF] = RST(0x28);

opcodes[0xF0] = LDH(A, Immediate); // LD A,(FF00+n)
opcodes[0xF1] = POP(A, FLAGS);
opcodes[0xF2] = LDC(A, C); // LD A,(FF00+C)
opcodes[0xF3] = DI;
opcodes[0xF4] = UNUSED;
opcodes[0xF5] = PUSH(A, FLAGS);
opcodes[0xF6] = ALU(OR, A, Immediate);
opcodes[0xF7] = RST(0x30);
opcodes[0xF8] = LD_HL_SP_e; // LD HL,SP+e
opcodes[0xF9] = LD16();
opcodes[0xFA] = LD_FROM_MEM_PTR(A, Immediate); // LD A,(nn)
opcodes[0xFB] = EI;
opcodes[0xFC] = UNUSED;
opcodes[0xFD] = UNUSED;
opcodes[0xFE] = ALU(CP, A, Immediate);
opcodes[0xFF] = RST(0x38);


CBcodes[0x00] = SHIFT(RLC, B);
CBcodes[0x01] = SHIFT(RLC, C);
CBcodes[0x02] = SHIFT(RLC, D);
CBcodes[0x03] = SHIFT(RLC, E);
CBcodes[0x04] = SHIFT(RLC, H);
CBcodes[0x05] = SHIFT(RLC, L);
CBcodes[0x06] = SHIFT(RLC, HL);
CBcodes[0x07] = SHIFT(RLC, A);
CBcodes[0x08] = SHIFT(RRC, B);
CBcodes[0x09] = SHIFT(RRC, C);
CBcodes[0x0A] = SHIFT(RRC, D);
CBcodes[0x0B] = SHIFT(RRC, E);
CBcodes[0x0C] = SHIFT(RRC, H);
CBcodes[0x0D] = SHIFT(RRC, L);
CBcodes[0x0E] = SHIFT(RRC, HL);
CBcodes[0x0F] = SHIFT(RRC, A);

CBcodes[0x10] = SHIFT(RL, B);
CBcodes[0x11] = SHIFT(RL, C);
CBcodes[0x12] = SHIFT(RL, D);
CBcodes[0x13] = SHIFT(RL, E);
CBcodes[0x14] = SHIFT(RL, H);
CBcodes[0x15] = SHIFT(RL, L);
CBcodes[0x16] = SHIFT(RL, HL);
CBcodes[0x17] = SHIFT(RL, A);
CBcodes[0x18] = SHIFT(RR, B);
CBcodes[0x19] = SHIFT(RR, C);
CBcodes[0x1A] = SHIFT(RR, D);
CBcodes[0x1B] = SHIFT(RR, E);
CBcodes[0x1C] = SHIFT(RR, H);
CBcodes[0x1D] = SHIFT(RR, L);
CBcodes[0x1E] = SHIFT(RR, HL);
CBcodes[0x1F] = SHIFT(RR, A);

CBcodes[0x20] = SHIFT(SLA, B);
CBcodes[0x21] = SHIFT(SLA, C);
CBcodes[0x22] = SHIFT(SLA, D);
CBcodes[0x23] = SHIFT(SLA, E);
CBcodes[0x24] = SHIFT(SLA, H);
CBcodes[0x25] = SHIFT(SLA, L);
CBcodes[0x26] = SHIFT(SLA, HL);
CBcodes[0x27] = SHIFT(SLA, A);
CBcodes[0x28] = SHIFT(SRA, B);
CBcodes[0x29] = SHIFT(SRA, C);
CBcodes[0x2A] = SHIFT(SRA, D);
CBcodes[0x2B] = SHIFT(SRA, E);
CBcodes[0x2C] = SHIFT(SRA, H);
CBcodes[0x2D] = SHIFT(SRA, L);
CBcodes[0x2E] = SHIFT(SRA, HL);
CBcodes[0x2F] = SHIFT(SRA, A);

CBcodes[0x30] = SWAP(B);
CBcodes[0x31] = SWAP(C);
CBcodes[0x32] = SWAP(D);
CBcodes[0x33] = SWAP(E);
CBcodes[0x34] = SWAP(H);
CBcodes[0x35] = SWAP(L);
CBcodes[0x36] = SWAP(HL);
CBcodes[0x37] = SWAP(A);
CBcodes[0x38] = SHIFT(SRL, B);
CBcodes[0x39] = SHIFT(SRL, C);
CBcodes[0x3A] = SHIFT(SRL, D);
CBcodes[0x3B] = SHIFT(SRL, E);
CBcodes[0x3C] = SHIFT(SRL, H);
CBcodes[0x3D] = SHIFT(SRL, L);
CBcodes[0x3E] = SHIFT(SRL, HL);
CBcodes[0x3F] = SHIFT(SRL, A);

for (var i = 0; i < 8; i++) {
    for (var j = 0; j < 8; j++) {
        CBcodes[0x40 + i * 8 + j] = BIT(i, j);
        CBcodes[0x80 + i * 8 + j] = RES(i, j);
        CBcodes[0xC0 + i * 8 + j] = SET(i, j);
    }
}


// This is the boot code that loads the Nintendo logo
var bootCode = "31 FE FF AF 21 FF 9F 32 CB 7C 20 FB 21 26 FF 0E 11 3E 80 32 E2 0C 3E F3 E2 32 3E 77 77 3E FC E0 47 11 04 01 21 10 80 1A CD 95 00 CD 96 00 13 7B FE 34 20 F3 11 D8 00 06 08 1A 13 22 23 05 20 F9 3E 19 EA 10 99 21 2F 99 0E 0C 3D 28 08 32 0D 20 F9 2E 0F 18 F3 67 3E 64 57 E0 42 3E 91 E0 40 04 1E 02 0E 0C F0 44 FE 90 20 FA 0D 20 F7 1D 20 F2 0E 13 24 7C 1E 83 FE 62 28 06 1E C1 FE 64 20 06 7B E2 0C 3E 87 E2 F0 42 90 E0 42 15 20 D2 05 20 4F 16 20 18 CB 4F 06 04 C5 CB 11 17 C1 CB 11 17 05 20 F5 22 23 22 23 C9 CE ED 66 66 CC 0D 00 0B 03 73 00 83 00 0C 00 0D 00 08 11 1F 88 89 00 0E DC CC 6E E6 DD DD D9 99 BB BB 67 63 6E 0E EC CC DD DC 99 9F BB B9 33 3E 3C 42 B9 A5 B9 A5 42 3C 21 04 01 11 A8 00 1A 13 BE 20 FE 23 7D FE 34 20 F5 06 19 78 86 23 05 20 FB 86 20 FE 3E 01 E0 50".split(" ").map(x => parseInt(x, 16));

function triggerInterrupt(vector) {
    cpu_halted = false;
    writeMem16(SP -= 2, PC >> 8, PC & 0xFF);
    PC = vector;
    IME = false;
    return 20;
}

function cpu() {
    var cycles = 4;
    if (!cpu_halted) {
        cycles = opcodes[readMem(PC)]();
    }

    if ((divPrescaler += cycles) > 255) {
        divPrescaler -= 256;
        MEM[0xFF04]++;
    }
    if (timerEnable) {
        if (1023 < (timerPrescaler += cycles * timerSpeed)) {
            timerPrescaler -= 1024;
            if (MEM[0xFF05]++ === 0xFF) {
                MEM[0xFF05] = MEM[0xFF06];
                MEM[0xFF0F] |= 1 << 2;
            }
        }
    }

    if (LCD_enabled) {
        LCD_scan += cycles;
        
        var mode = 0, coincidence = false, draw = false;
        if (LCD_scan <= 80) mode = 2;
        else if (LCD_scan <= 252) mode = 3;
        else if (LCD_scan < 456) mode = 0;
        else {
            mode = 2;
            LCD_scan -= 456;
            MEM[0xFF44]++;
            draw = true;
            if (MEM[0xFF44] > 153) MEM[0xFF44] = 0;
            coincidence = (MEM[0xFF44] === MEM[0xFF45]);
        }

        if (MEM[0xFF44] >= 144) mode = 1; // V-Blank
        else if (draw) {
            var LY = MEM[0xFF44];
            //  FF40 - LCDC - LCD Control (R/W)
            //
            //  Bit 7 - LCD Display Enable             (0=Off, 1=On)
            //  Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
            //  Bit 5 - Window Display Enable          (0=Off, 1=On)
            //  Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
            //  Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
            //  Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
            //  Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
            //  Bit 0 - BG Display (for CGB see below) (0=Off, 1=On)
            var baseTileOffset, tileSigned;
            // Tile Data Select
            if (MEM[0xFF40] & (1 << 4)) {
                baseTileOffset =  0x8000;
                tileSigned = false;
            } else {
                baseTileOffset =  0x9000;
                tileSigned = true;
            }
            var bgpalette = [
              (MEM[0xFF47]) & 3,
              (MEM[0xFF47] >> 2) & 3,
              (MEM[0xFF47] >> 4) & 3,
              (MEM[0xFF47] >> 6) & 3
            ]
      
            function grabTile(n, offset) {
                if (!tileSigned){
                    var tileptr = offset + n * 16;
                } else {
                    var tileptr = offset + signedOffset(n) * 16;
                }
                var d1 = MEM[tileptr], d2 = MEM[tileptr + 1]
                return pixelDecoder[d1][d2]
            }
            if (MEM[0xFF40] & 1) {

                var bgTileMapAddr = MEM[0xFF40] & (1 << 3) ? 0x9C00 : 0x9800;

                var x = MEM[0xFF43] >> 3;
                var xoff = MEM[0xFF43] & 7;
                var y = (LY + MEM[0xFF42]) & 0xFF;
                bgTileMapAddr += (~~(y / 8)) * 32;

                var tileOffset = baseTileOffset + (y & 7) * 2;

                var pix = grabTile(MEM[bgTileMapAddr + x], tileOffset);
                var dpy = LY * 160;

                for (var i = 0; i < 160; i++) {
                    dpixels[dpy + i] = bgpalette[pix[xoff++]];

                    if (xoff === 8) {
                        x = (x + 1) & 0x1F;
                        pix = grabTile(MEM[bgTileMapAddr + x], tileOffset);
                        xoff = 0;
                    }
                }
            }
 
            // FF4A - WY
            // FF4B - WX
      
            if ( (MEM[0xFF40] & (1<<5)) && LY >= MEM[0xFF4A]) { // Window display enable
                // Window Tile map display select
                var wdTileMapAddr = MEM[0xFF40] & (1 << 6) ? 0x9C00 : 0x9800;
      
                var xoff = 0;
                var y = LY - MEM[0xFF4A];
      
                wdTileMapAddr += (~~(y / 8)) * 32; 
                var tileOffset = baseTileOffset + (y & 7) * 2;
      
                pix = grabTile(MEM[wdTileMapAddr], tileOffset);
      
                for (var i = MEM[0xFF4B] - 7; i < 160; i++) {
                    dpixels[dpy + i] = bgpalette[pix[xoff++]]
                    if (xoff === 8) {
                        pix = grabTile(MEM[++wdTileMapAddr], tileOffset);
                        xoff = 0;
                    }
                }
      
            }
      
            if (MEM[0xFF40] & 2) { // Sprite display enabled
                // Render sprites
                var height, tileNumMask;
                if (MEM[0xFF40] & (1 << 2)) {
                    height = 16;
                    tileNumMask = 0xFE; // in 8x16 mode, lowest bit of tile number is ignored
                } else {
                    height=8;
                    tileNumMask=0xFF; 
                }
        
                var OBP0 = [
                    0,
                    (MEM[0xFF48] >> 2) & 3,
                    (MEM[0xFF48] >> 4) & 3,
                    (MEM[0xFF48] >> 6) & 3
                ];
                var OBP1 = [
                    0,
                    (MEM[0xFF49] >> 2) & 3,
                    (MEM[0xFF49] >> 4) & 3,
                    (MEM[0xFF49] >> 6) & 3
                ];
                // OAM 4 bytes per sprite, 40 sprites
                for (var i = 0xFE00; i < 0xFEA0; i += 4) {
                    var ypos = MEM[i] - 16 + height;
                    if (LY >= ypos-height && LY < ypos) {
        
                        var xpos = MEM[i + 1],
                            tileNum = 0x8000 + (MEM[i + 2] & tileNumMask) * 16,
                            att = MEM[i + 3];
                    
                        // Bit7   OBJ-to-BG Priority (0=OBJ Above BG, 1=OBJ Behind BG color 1-3)
                        //        (Used for both BG and Window. BG color 0 is always behind OBJ)
                        // Bit6   Y flip          (0=Normal, 1=Vertically mirrored)
                        // Bit5   X flip          (0=Normal, 1=Horizontally mirrored)
                        // Bit4   Palette number  **Non CGB Mode Only** (0=OBP0, 1=OBP1)
        
                        var palette = att & (1 << 4) ? OBP1 : OBP0;
                        var behind = att & (1 << 7);
        
                        if (att & (1 << 6)) { 
                            // Y flip
                            tileNum += (ypos - LY - 1) * 2
                        } else {
                            tileNum += (LY - ypos + height) * 2 
                        }
                        var d1= MEM[tileNum], d2= MEM[tileNum + 1],
                            row = pixelDecoder[d1][d2];
                        if (att & (1 << 5)) { 
                            // X flip
                            if (behind) {
                                for (var j = Math.max(1 - xpos, 0); j < 8; j++) {
                                    if (dpixels[dpy + xpos - 1 - j] == 0) dpixels[dpy + xpos - 1 - j] = palette[row[j]];
                                }
                            } else {
                                for (var j = Math.max(1 - xpos, 0); j < 8; j++) {
                                    if (row[j]) dpixels[dpy + xpos - 1 - j] = palette[row[j]];
                                }
                            }
                        } else {
                            if (behind) {
                                for (var j = Math.max(8 - xpos, 0); j < 8; j++) { 
                                    if (dpixels[dpy + xpos - 8 + j] == 0) dpixels[dpy + xpos - 8 + j] = palette[row[j]];
                                }
                            } else {
                                for (var j = Math.max(8 - xpos, 0); j < 8; j++) {
                                    if (row[j]) dpixels[dpy + xpos - 8 + j] = palette[row[j]];
                                }
                            }
                        
                        }
                    }
                }               
            }
        }

        if (coincidence) {
            if (MEM[0xFF41] & (1 << 6)) {
                MEM[0xFF0F] |= 1 << 1;
                MEM[0xFF41] |= 1 << 2;
            }
        } else MEM[0xFF41] &= 0xFB;

        if (LCD_lastmode != mode) {
            if (mode === 0) {
                if (MEM[0xFF41] & (1 << 3)) MEM[0xFF0F] |= 1 << 1;
            } else if (mode === 1) {
                if (MEM[0xFF41] & (1 << 4)) MEM[0xFF0F] |= 1 << 1;
                if (MEM[0xFFFF] & 1) MEM[0xFF0F] |= 1 << 0;
                renderDisplayCanvas();
            } else if (mode === 2) {
                if (MEM[0xFF41] & (1 << 5)) MEM[0xFF0F] |= 1 << 1;
            }

            MEM[0xFF41] &= 0xF8;
            MEM[0xFF41] += mode;
            LCD_lastmode = mode;
        }
    }

    if (IME) {
        var i = MEM[0xFF0F] & MEM[0xFFFF];

        if (i & (1 << 0)) {
            MEM[0xFF0F] &= ~(1 << 0);
            cycles += triggerInterrupt(0x40);
        } else if (i & (1 << 1)) {
            MEM[0xFF0F] &= ~(1 << 1);
            cycles += triggerInterrupt(0x48);
        } else if (i & (1 << 2)) {
            MEM[0xFF0F] &= ~(1 << 2);
            cycles += triggerInterrupt(0x50);
        } else if (i & (1 << 3)) {
            MEM[0xFF0F] &= ~(1 << 3);
            cycles += triggerInterrupt(0x58);
        } else if (i & (1 << 4)) {
            MEM[0xFF0F] &= ~(1 << 4);
            cycles += triggerInterrupt(0x60);
        }
    } else cpu_halted = false;

    if (PC === 0x100) {
        instr_log = true;
    }
    instr_count += cycles;
    return cycles;
}


var requestStop = false;
var targ = 0x8e
function runto(end){
  requestStop = false;

  var tcount=0;
  targ = end ? end : parseInt(prompt('Address', f(targ,4)),16); 

  run()
}

const frameClocks = 4194304/60;
var frameCountdown = frameClocks;

function run() {
    while (true) {
        var cycles = cpu();
        frameCountdown -= cycles;

        if (frameCountdown < 0){
            frameCountdown += frameClocks;
            break;
        }
    }
    if (PC != targ && !requestStop) window.requestAnimationFrame(run)
    debugData()
}



const ioMap = {
    0xFF00: "P1/JOYP ",
    0xFF01: "SB ",
    0xFF02: "SC ",
    0xFF04: "DIV ",
    0xFF05: "TIMA ",
    0xFF06: "TMA ",
    0xFF07: "TAC ",
    0xFF0F: "IF ",
    0xFF10: "NR10 ",
    0xFF11: "NR11 ",
    0xFF12: "NR12 ",
    0xFF13: "NR13 ",
    0xFF14: "NR14 ",
    0xFF16: "NR21 ",
    0xFF17: "NR22 ",
    0xFF18: "NR23 ",
    0xFF19: "NR24 ",
    0xFF1A: "NR30 ",
    0xFF1B: "NR31 ",
    0xFF1C: "NR32 ",
    0xFF1D: "NR33 ",
    0xFF1E: "NR34 ",
    0xFF20: "NR41 ",
    0xFF21: "NR42 ",
    0xFF22: "NR43 ",
    0xFF23: "NR44 ",
    0xFF24: "NR50 ",
    0xFF25: "NR51 ",
    0xFF26: "NR52 ",
    0xFF3F: "Wave Pattern",
    0xFF40: "LCDC ",
    0xFF41: "STAT ",
    0xFF42: "SCY ",
    0xFF43: "SCX ",
    0xFF44: "LY ",
    0xFF45: "LYC ",
    0xFF46: "DMA ",
    0xFF47: "BGP ",
    0xFF48: "OBP0 ",
    0xFF49: "OBP1 ",
    0xFF4A: "WY ",
    0xFF4B: "WX ",
    // 0xFF4D: "KEY1 ",
    // 0xFF4F: "VBK ",
    // 0xFF51: "HDMA1 ",
    // 0xFF52: "HDMA2 ",
    // 0xFF53: "HDMA3 ",
    // 0xFF54: "HDMA4 ",
    // 0xFF55: "HDMA5 ",
    // 0xFF56: "RP ",
    // 0xFF68: "BCPS/BGPI ",
    // 0xFF69: "BCPD/BGPD ",
    // 0xFF6A: "OCPS/OBPI ",
    // 0xFF6B: "OCPD/OBPD ",
    // 0xFF6C: "Undocumented (FEh) ",
    // 0xFF70: "SVBK ",
    // 0xFF72: "Undocumented (00h) ",
    // 0xFF73: "Undocumented (00h) ",
    // 0xFF74: "Undocumented (00h) ",
    // 0xFF75: "Undocumented (8Fh) ",
    // 0xFF76: "Undocumented (00h) ",
    // 0xFF77: "Undocumented (00h) ",
    0xFFFF: "IE "
}

function f(a,l) {return ("0000"+a.toString(16).toUpperCase()).slice(-l||-2)}
(debugData = function() {
    var debugOffset = PC & 0xFF00;

    var memoryHtml = "";
    for (var j = 0; j < 16; j++) {
        memoryHtml += "$" + f(debugOffset + j * 16, 4) + "   ";
        for (var i = 0; i < 16; i++) {
            var q = i + j * 16 + debugOffset;
            memoryHtml += "<span title='$" + f(q, 4) + "'" +
                (PC === q ? " style='color:red'>" : ">") +
                f(readMem(q)) + "</span>" +
                (i === 7 ? " | " : " ");
        }
        memoryHtml += "\n";        
    }

    memoryHtml += "\nAF: " + f(REG[A]) + f((FLAGS.Z << 7) + (FLAGS.N << 6) + (FLAGS.H << 5) + (FLAGS.C << 4)) + "        Z: " + FLAGS.Z;
    memoryHtml += "\nBC: " + f(REG[B]) + f(REG[C]) + "        N: " + FLAGS.N;
    memoryHtml += "\nDE: " + f(REG[D]) + f(REG[E]) + "        H: " + FLAGS.H;
    memoryHtml += "\nHL: " + f(REG[H]) + f(REG[L]) + "        C: " + FLAGS.C;
    memoryHtml += "\nSP: " + f(SP, 4);
    memoryHtml += "\nPC: " + f(PC, 4);
    memoryHtml += "\n\nCycle Count: " + instr_count;
    document.getElementById('memory').innerHTML = memoryHtml;

    var ioHtml = "";
    for (var i in ioMap) {
        var r = f(readMem(i));
        ioHtml += "$" + f(i * 1, 4) + ": " + (r === 0 ? "--" : r) + " - " + ioMap[i] + " \n";
    }
    document.getElementById('io').innerHTML = ioHtml;

    var stackHtml = "";
    for (var i = SP + 20; i >= SP - 20; i -= 2) {
        if (i > 0xFFFE || i < 0) {
            stackHtml += " ----  ----\n";
        } else {
            var r = f(readMem(i + 1)) + f(readMem(i));
            stackHtml += "$" + f(i, 4) + ": " + r + (i === SP ? "*" : "") + " \n";
        }
    }
    document.getElementById('stack').innerHTML = stackHtml;
})();



var openFile = function(event) {
  var input = event.target;

  var reader = new FileReader();
  reader.onload = function(){
    ROM = new Uint8Array(reader.result);
    FirstROMPage = ROM.slice(0,256)



    // Overwrite first page with bootcode
    for (var i = 0;i < 256; i++) ROM[i]=bootCode[i];


    // According to BGB
    MEM[0xFF41] = 1;
    MEM[0xFF43] = 0;

    // In case a new ROM is loaded!
    ROMbank = 1; ROMbankoffset = (ROMbank - 1) * 0x4000;
    RAMbank = 0; RAMbankoffset = RAMbank * 0x2000 - 0xA000;
    RAMenabled = false; MBCRamMode = 0;
    divPrescaler = 0, timerPrescaler = 0, timerLength = 1, timerEnable = false;
    LCD_enabled = false, LCD_lastmode = 1, LCD_scan = 0;
    PC = 0, SP = 0, IME = false, cpu_halted = false;

    debugData();
  };
  reader.readAsArrayBuffer(input.files[0]);
};