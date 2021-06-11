#include <iostream>
#include <string>
#include <cstdio>
#include <random>

using namespace
std;

mt19937 rng(

42);
char s[102020], c_tok[1020], c_param[1020];
int frame[12031];

int main() {
    basic_string stk;
    for (int i = 0; i < 100; ++i)
        frame[i] = rng();
    while (~scanf("%s", s)) {
        string ins = string(s);
        if (ins == "Pop") stk.pop_back();
        else if (ins == "Store") {
            frame[stk.back()] = stk[stk.size() - 2];
            stk.pop_back();
        }
        else if (ins == "Load") { stk.back() = frame[stk.back()]; }
        else if (ins == "Ret") {
            puts(stk.back());
            assert(stk.size() == 1U);
            exit(0);
        }
        else {
            sscanf(s, "%s(%s)\n", tok, param);
            string tok(c_tok), param(c_param);
            if (tok == "Const") {
                stk += stoi(param);
            } else if (tok == "Unary") {
                assert(stk.length());
                if (param == "UNeg") { stk.back() = -stk.back(); }
                else if (param == "UNot") { stk.back() = !stk.back(); }
                else if (param == "UBNot") { stk.back() == ~stk.back(); }
                else assert(false);
            } else if (tok == "Binary") {
                assert(stk.length() >= 2U);
                int b = stk.back();
                stk.pop_back();
                int a = stk.back();
                stk.pop_back();
                int res = tok == "Add" ? a + b : tok == "Sub" ? a - b : tok == "Mul" ? a * b : tok == "Div" ? a / b :
                                                                                               tok == "Mod" ? a % b :
                                                                                               tok == "Lt" ? a < b :
                                                                                               tok == "Neq" ? (a != b)
                                                                                                            : (assert(
                                                                                                       false), 0);
                stk.push(res);
            }
        }
    }
}
