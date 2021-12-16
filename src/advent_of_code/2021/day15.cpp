#include <bits/stdc++.h>

using namespace std;

#define F first
#define S second

typedef pair<int, int> ii;

const int oo = (1 << 30);
const int MaxN = 250000 + 5;
int n;
vector<ii> g[MaxN];

priority_queue<ii, vector<ii>, greater<ii>> pq;

vector<int> Dijkstra(int s) {
  vector<int> d(n, oo);
  pq = priority_queue<ii, vector<ii>, greater<ii>>();

  d[s] = 0;
  pq.push(ii(0, s));

  while (!pq.empty()) {
    int dist = pq.top().F, u = pq.top().S;
    pq.pop();

    if (dist == d[u]) {
      for (int i = 0; i < (int)g[u].size(); ++i) {
        int v = g[u][i].S;
        int w = g[u][i].F;
        if (d[u] + w < d[v]) {
          d[v] = d[u] + w;
          pq.push(ii(d[v], v));
        }
      }
    }
  }
  return d;
}

bool is_inside(int x, int y, int row, int col) {
  return x >= 0 && x < row && y >= 0 && y < col;
}

int dx[] = {0, 1, 0, -1};
int dy[] = {-1, 0, 1, 0};

vector<ii> neighbors(int i, int j, int row, int col) {
  vector<ii> ans;
  for (int k = 0; k < 4; ++k) {
    int x = i + dx[k];
    int y = j + dy[k];
    if (is_inside(x, y, row, col))
      ans.push_back(ii(x, y));
  }
  return ans;
}

int one(int x, int y, int row) { return x * row + y; }

int solve(vector<vector<int>> grid, int row, int col) {
  n = row * col;

  for (int i = 0; i < row; ++i)
    for (int j = 0; j < col; ++j)
      for (ii adj : neighbors(i, j, row, col))
        g[one(i, j, row)].push_back(
            ii(grid[adj.F][adj.S], one(adj.F, adj.S, row)));

  vector<int> d = Dijkstra(0);

  return d[one(row - 1, col - 1, row)];
}

int main() {
  ifstream infile("../../../resources/2021/day15-sample.txt");

  vector<vector<int>> grid;
  string line;
  while (getline(infile, line)) {
    vector<int> items;
    for (int i = 0; i < line.length(); ++i)
      items.push_back(line[i] - '0');
    grid.push_back(items);
  }

  int row = grid.size();
  int col = grid[0].size();

  cout << "part1: " << solve(grid, row, col) << endl;

  int row5 = row * 5;
  int col5 = col * 5;
  vector<vector<int>> grid5(row5, vector<int>(col5));

  for (int i = 0; i < row5; ++i)
    for (int j = 0; j < col5; ++j)
      if (i < row && j < col)
        grid5[i][j] = grid[i][j];
      else if (i >= row && j >= row)
        grid5[i][j] = (grid5[i - row][j - col] + 1) % 10;
      else if (i >= row)
        grid5[i][j] = (grid5[i - row][j] + 1) % 10;
      else
        grid5[i][j] = (grid5[i][j - col] + 1) % 10;

  cout << "part2: " << solve(grid5, row5, col5) << endl;

  return 0;
}
