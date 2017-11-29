const int g_display = 0;

#define g_largestKey 10000
#define g_digits 3
#define g_blobLength (1048576*10)

const unsigned long g_mapSize = 17592186040320;

// A blob is a sequence of characters (no trailing '\0').
// A blob is not a string.
const char *g_blobs[10];

const char g_fileName[] = "lmdb_test.txt";

const char g_dbDirName[] = "lmdbtest";
const char g_dbName[] = "lmdbtest1";


/*
  $ gcc -c lmdb_test.c
  $ gcc -lm -pthread lmdb_test.o liblmdb.a -o lmdb_test.out
  $ ./lmdb_test.out
*/

#include "lmdb_code.h"


unsigned long NextKey(unsigned long usedKeys[])
{
  // check that usedKeys[] elements are not all non-zero
  unsigned long r = rand() % g_largestKey;
  while (usedKeys[r] != 0)
    r = rand() % g_largestKey;
  usedKeys[r] = 1;
  return r;
}

void ResetUsedKeys(unsigned long usedKeys[])
{
  unsigned long i = 0; for (i = 0; i < g_largestKey; ++i) usedKeys[i] = 0;
}

long MillisSinceEpoch()
{
  struct timeval t;
  gettimeofday(&t, NULL);
  return round((t.tv_sec + (t.tv_usec / 1000000.0)) * 1000.0);
}

void InitBlobs()
{
  unsigned long i = 0;
  for (i = 0; i < 10; ++i)
  {
    char *blob = (char *) malloc(g_blobLength);
    assert(blob);

    char c = '0' + i;

    unsigned long j = 0;
    for (j = 0; j < g_blobLength; ++j)
      blob[j] = c;

    g_blobs[i] = blob;
  }
  /*
  for (i = 0; i < 10; ++i)
    printf("%d: %c\n", i, g_blobs[i][0]);
  */
}

unsigned long *Init(char *testDir)
{
  char currDir[5012] = "";
  getcwd(currDir, 5011);
  char testPath[5012] = "";
  // remove data.mdb and lock.mdb
  sprintf(testPath, "%s/%s/%s", currDir, testDir, "data.mdb");
  remove(testPath);
  sprintf(testPath, "%s/%s/%s", currDir, testDir, "lock.mdb");
  remove(testPath);

  unsigned long *usedKeys = (unsigned long *) malloc(g_largestKey * sizeof(unsigned long));
  ResetUsedKeys(usedKeys);
  InitBlobs();

  srand(time(NULL)); // should only be called once

  return usedKeys;
}

void Output(char lines[][64], int lineCount)
{
  FILE *f = fopen(g_fileName, "a");
  int i = 0;
  for (i = 0; i <lineCount; ++i)
  {
    printf("%s\n", lines[i]);
    fprintf(f, "%s\n", lines[i]);
  }
  fclose(f);
}

void Output1(char lines[][64], int lineCount)
{
  FILE *f = fopen(g_fileName, "a");
  int i = 0;
  for (i = 0; i <lineCount; ++i)
  {
    printf("%s", lines[i]);
    fprintf(f, "%s", lines[i]);
  }
  fclose(f);
}

void Outcome(long startPut, long stopPut, long startGet, long stopGet)
{
  long putDuration = stopPut - startPut;
  long getDuration = stopGet - startGet;

  long duration = putDuration + getDuration;

  long putPerKey = -1;
  if (0 < putDuration) putPerKey = g_largestKey / putDuration;
  long getPerKey = -1;
  if (0 < getDuration) getPerKey = g_largestKey / getDuration;

  char lines[6][64];
  sprintf(lines[0], "duration: %ld", duration);
  sprintf(lines[1], "put: %ld", putDuration);
  sprintf(lines[2], "puts/ms: %ld", putPerKey);
  sprintf(lines[3], "get: %ld", getDuration);
  sprintf(lines[4], "gets/ms: %ld", getPerKey);
  sprintf(lines[5], "");
  Output(lines, 6);
}

void Assert(int rc, int line)
{
  if (rc != MDB_SUCCESS)
    printf("line: %d, rc is not MDB_SUCCESS, it is: %d\n", line, rc);
  assert(rc == MDB_SUCCESS);
}

unsigned long TreeSize(MDB_txn *txn, MDB_dbi *dbi)
{
  MDB_stat stat;
  int rc = mdb_stat(txn, *dbi, &stat); Assert(rc, __LINE__);
  return 4096 * (stat.ms_branch_pages + stat.ms_leaf_pages + stat.ms_overflow_pages);
}

void DisplayEnvStat(MDB_env *env, MDB_txn *txn, MDB_dbi *dbi)
{
  char s[20][64];

  if (env == NULL)
    return;

  MDB_envinfo envinfo;
  int rc = mdb_env_info(env, &envinfo); Assert(rc, __LINE__);
  sprintf(s[0], "envinfo.me_mapsize: %ld", envinfo.me_mapsize);
  sprintf(s[1], "envinfo.me_last_pgno: %ld", envinfo.me_last_pgno);
  sprintf(s[2], "envinfo.me_last_txnid: %ld", envinfo.me_last_txnid);
  sprintf(s[3], "envinfo.me_maxreaders: %ld", envinfo.me_maxreaders);
  sprintf(s[4], "envinfo.me_numreaders: %ld", envinfo.me_numreaders);

  if (txn == NULL || dbi == NULL)
  {
    Output(s, 5);
    return;
  }

  MDB_stat stat;
  rc = mdb_stat(txn, *dbi, &stat); Assert(rc, __LINE__);
  sprintf(s[5], "stat.ms_psize: %ld", stat.ms_psize);
  sprintf(s[6], "stat.ms_depth: %ld", stat.ms_depth);
  sprintf(s[7], "stat.ms_branch_pages: %ld", stat.ms_branch_pages);
  sprintf(s[8], "stat.ms_leaf_pages: %ld", stat.ms_leaf_pages);
  sprintf(s[9], "stat.ms_overflow_pages: %ld", stat.ms_overflow_pages);
  sprintf(s[10], "stat.ms_entries: %ld", stat.ms_entries);
  sprintf(s[11], "tree size: %ld", TreeSize(txn, dbi));
  sprintf(s[12], "");

  Output(s, 13);
}

void DisplayTreeInfo(MDB_txn *txn, MDB_dbi *dbi)
{
  MDB_stat stat;
  int rc = mdb_stat(txn, *dbi, &stat); Assert(rc, __LINE__);
  printf("stat.ms_depth: %ld\n", stat.ms_depth);
  printf("stat.ms_branch_pages: %ld\n", stat.ms_branch_pages);
  printf("stat.ms_leaf_pages: %ld\n", stat.ms_leaf_pages);
  printf("stat.ms_overflow_pages: %ld\n", stat.ms_overflow_pages);
  printf("\n");
}

unsigned long Nines(char *prefix, int nines)
{
  int prefixLen = strlen(prefix);
  assert(nines + prefixLen + 1 < 32);
  char n[32];
  sprintf(n, "%s", prefix);
  int i = 0;
  for (i = 0; i < nines; ++i)
    n[i+prefixLen] = '9';
  n[i+prefixLen] = '\0';
  return atoi(n);
}


void main(int argc, char * argv[])
{
  // printf("sizeof(mdb_size_t) = %ld\n\n", sizeof(mdb_size_t));

  unsigned long zero = Nines("0", g_digits);
  unsigned long one = Nines("1", g_digits);
  unsigned long two = Nines("2", g_digits);
  unsigned long three = Nines("3", g_digits);
  unsigned long four = Nines("4", g_digits);
  unsigned long five = Nines("5", g_digits);
  unsigned long six = Nines("6", g_digits);
  unsigned long seven = Nines("7", g_digits);
  unsigned long eight = Nines("8", g_digits);
  unsigned long nine = Nines("9", g_digits);

  char lines[4][64];
  sprintf(lines[0], "number of keys: %ld", g_largestKey);
  sprintf(lines[1], "values/key: 1");
  sprintf(lines[2], "blob length: %ld", g_blobLength);
  sprintf(lines[3], "map size: %ld", g_mapSize);
  Output(lines, 4);

  char testDir[] = "lmdb";

  // code modified from mtest*.c
  MDB_env *env = NULL;
  MDB_dbi dbi;
  MDB_val key, value;
  MDB_txn *txn = NULL;
  MDB_stat mst;
  MDB_cursor *cursor = NULL;
  unsigned long kBuf = -1;
  char *vBuf = (char *) malloc(g_blobLength);
  
  key.mv_size = sizeof(kBuf);
  key.mv_data = &kBuf;
  value.mv_size = g_blobLength;
  // value.mv_data = NULL;
  value.mv_data = vBuf;

  unsigned long *usedKeys = Init(testDir);

  int rc = mdb_env_create(&env); Assert(rc, __LINE__);
  rc = mdb_env_set_mapsize(env, g_mapSize); Assert(rc, __LINE__);
  rc = mdb_env_set_maxdbs(env, 1); /* necessary */ Assert(rc, __LINE__);
  rc = mdb_env_set_maxreaders(env, 1); Assert(rc, __LINE__);
  rc = mdb_env_open(env, "./db", MDB_WRITEMAP, 0664); Assert(rc, __LINE__);
  rc = mdb_txn_begin(env, NULL, 0, &txn); Assert(rc, __LINE__);
  rc = mdb_dbi_open(txn, g_dbDirName, MDB_CREATE, &dbi); Assert(rc, __LINE__);
  rc = mdb_cursor_open(txn, dbi, &cursor); Assert(rc, __LINE__);

  printf("\nBegin\n");
  DisplayEnvStat(env, txn, &dbi);

  long startPut = MillisSinceEpoch();

  unsigned long putPrev = startPut;

  unsigned long keyCount = 0;
  for (keyCount = 0; keyCount < g_largestKey; ++keyCount)
  {
    unsigned long iKey = NextKey(usedKeys);

    kBuf = iKey;
    if (g_display) printf("key: %ld, values: ", kBuf);

    // necessary because mbd_cursor_get() changes key.mv_data
    key.mv_data = &kBuf;
    kBuf = iKey;

    value.mv_data = (void *) g_blobs[iKey % 10];
    if (g_display) printf("%c\n", g_blobs[iKey % 10][0]);

    rc = mdb_cursor_put(cursor, &key, &value, 0); Assert(rc, __LINE__);

    if (keyCount == zero || keyCount == one || keyCount == two || keyCount == three ||
        keyCount == four || keyCount == five || keyCount == six ||
        keyCount == seven || keyCount == eight || keyCount == nine) 
    {
      unsigned long putNow = MillisSinceEpoch();
      char lines[3][64];
      sprintf(lines[0], "after put %ld: ", keyCount);
      sprintf(lines[1], "tree size: %ld: ", TreeSize(txn, &dbi));
      sprintf(lines[2], "time: %ld\n", putNow - putPrev);
      Output1(lines, 3);
      putPrev = putNow;
    }
  }
printf("\n");
  if (g_display) printf("\n");

  long stopPut = MillisSinceEpoch();

  ResetUsedKeys(usedKeys);

  long startGet = MillisSinceEpoch();

  printf("After puts, before gets\n");
  DisplayEnvStat(env, txn, &dbi);

  keyCount = 0;
  for (keyCount = 0; keyCount < g_largestKey; ++keyCount)
  {
    unsigned long iKey = NextKey(usedKeys);

    // necessary because mbd_cursor_get() changes key.mv_data
    key.mv_data = &kBuf;
    kBuf = iKey;

    rc = mdb_cursor_get(cursor, &key, &value, MDB_SET_KEY);
    if (rc != MDB_SUCCESS)
    {
      if (g_display) printf("get key '%ld' not found, error: %d\n", iKey, rc);
      continue;
    }

    rc = mdb_cursor_get(cursor, &key, &value, MDB_GET_CURRENT);
    if (rc == MDB_NOTFOUND)
    {
      if (g_display) printf("get key '%ld' not seekable\n", iKey);
      continue;
    }

    if (g_display)
      printf("get key: %ld, values: %c\n", *((int *) key.mv_data), *((char *) value.mv_data));
  }
  if (g_display) printf("\n");

  long stopGet = MillisSinceEpoch();

  printf("End\n");
  DisplayEnvStat(env, txn, &dbi);

  Outcome(startPut, stopPut, startGet, stopGet);

  mdb_cursor_close(cursor);
  mdb_txn_abort(txn);
  // mdb_dbi_close(env, dbi);
  mdb_env_close(env);
}

