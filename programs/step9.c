int gcd(int a, int b) {
    return b == 0 ? a : gcd(a, b % a);
}

int f() {}

int main() {
    int a = 114514;
    int b = 1919810;
    return gcd(a, b);
}