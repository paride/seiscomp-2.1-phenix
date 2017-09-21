/***************************************************************************** 
 * font.h
 *
 * (c) 1996, 2002 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

static const unsigned char *const alpha_font[]={
/* ! */ "\230\232\136",
/* " */ "\225\126\245\146",
/* # */ "\202\162\204\164\220\126\240\146",
/* $ */ "\230\136\211\151\162\153\113\104\115\155",
/* % */ "\200\166\106\104\124\126\240\160\162\142\140",
/* & */ "\262\140\110\101\102\145\146\126\125\160",
/* ' */ "\225\126",
/* ( */ "\230\121\125\136",
/* ) */ "\230\141\145\136",
/* * */ "\230\136\211\155\251\115",
/* + */ "\231\135\213\153",
/* , */ "\220\131\133\123\122\132",
/* - */ "\213\153",
/* . */ "\220\130\131\121\120",
/* / */ "\200\166",
/* 0 */ "\200\166\220\140\162\164\146\126\104\102\120",
/* 1 */ "\220\140\230\136\125",
/* 2 */ "\205\116\156\165\164\153\113\102\100\160",
/* 3 */ "\201\110\150\161\162\153\133\253\164\165\156\116\105",
/* 4 */ "\206\103\163\245\140",
/* 5 */ "\201\110\150\161\162\153\103\106\166",
/* 6 */ "\202\113\153\162\161\150\110\101\105\116\146",
/* 7 */ "\205\106\166\165\133\130",
/* 8 */ "\210\150\161\162\153\113\102\101\110\213\104\105\116\156\165\164\153",
/* 9 */ "\201\110\150\161\165\156\116\105\104\113\153\164",
/* : */ "\221\131\132\122\121\224\134\135\125\124",
/* ; */ "\230\132\122\121\131\224\134\135\125\124",
/* < */ "\240\103\146",
/* = */ "\212\162\214\164",
/* > */ "\220\163\126",
/* ? */ "\205\116\156\165\164\153\133\132\230",
/* @ */ "\210\101\105\116\156\165\162\151\131\122\123\134\144\153\152",
/* A */ "\200\103\136\163\160\203\163",
/* B */ "\200\150\161\162\153\113\253\164\165\156\106\216\110",
/* C */ "\265\156\116\105\101\110\150\161",
/* D */ "\200\150\161\165\156\106\216\110",
/* E */ "\266\106\100\160\203\133",
/* F */ "\266\106\100\203\133",
/* G */ "\265\156\116\105\101\110\150\161\260\162\153\143",
/* H */ "\200\106\260\166\203\163",
/* I */ "\210\150\216\156\230\136",
/* J */ "\202\101\110\140\151\156\246\166",
/* K */ "\200\106\203\166\224\160",
/* L */ "\206\100\160",
/* M */ "\200\106\133\166\160",
/* N */ "\200\106\160\166",
/* O */ "\201\110\150\161\165\156\116\105\101",
/* P */ "\200\106\156\165\164\153\103",
/* Q */ "\201\110\140\162\165\156\116\105\101\242\160",
/* R */ "\200\106\156\165\164\153\103\223\150",
/* S */ "\201\110\150\161\162\153\113\104\105\116\156\165",
/* T */ "\206\166\236\130",
/* U */ "\206\101\110\150\161\166",
/* V */ "\206\130\166",
/* W */ "\206\110\135\150\166",
/* X */ "\200\166\206\160",
/* Y */ "\206\133\166\233\130",
/* Z */ "\206\166\100\160",
/* [ */ "\240\120\126\146",
/* \ */ "\206\160",
/* ] */ "\220\140\146\126",
/* ^ */ "\204\136\164",
/* _ */ "\200\160",
/* ` */ "\236\145",
/* a */ "\214\154\163\160\110\101\112\162",
/* b */ "\206\100\150\161\163\154\104",
/* c */ "\263\154\114\103\101\110\150",
/* d */ "\266\160\110\101\103\114\164",
/* e */ "\202\162\163\154\114\103\101\110\150",
/* f */ "\265\156\136\125\120\204\144",
/* g */ "\200\160\163\154\114\103\112\162\254\165",
/* h */ "\200\106\204\154\163\160",
/* i */ "\200\140\220\124\114",
/* j */ "\201\110\150\161\164",
/* k */ "\200\106\202\154\223\160",
/* l */ "\200\140\220\126\106",
/* m */ "\200\104\203\114\124\133\130\233\144\154\163\160",
/* n */ "\200\104\203\114\154\163\160",
/* o */ "\201\110\150\161\163\154\114\103\101",
/* p */ "\200\104\154\163\152\102",
/* q */ "\201\110\140\162\163\154\114\103\101\242\160",
/* r */ "\210\113\124\154\163\213\104",
/* s */ "\210\150\161\152\112\103\114\164",
/* t */ "\261\150\130\121\126\204\144",
/* u */ "\204\101\110\160\164",
/* v */ "\204\103\130\163\164",
/* w */ "\204\101\110\132\150\161\164",
/* x */ "\200\164\204\160",
/* y */ "\204\103\112\152\163\164\263\161\150\100",
/* z */ "\204\164\100\160",
/* { */ "\266\146\135\134\123\132\131\140\160\213\123",
/* | */ "\230\132\234\136",
/* } */ "\206\126\135\134\143\132\131\120\100\243\153",
/* ~ */ "\203\125\143\165"
};