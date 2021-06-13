int a[4][4];
int main() {
    if (&a[2][2] != &((int *) a)[2 * 4 + 2]) return 4;
    if (&a[3][3] == &((int **) a)[3][3]) return 5;
    return 0;
}
