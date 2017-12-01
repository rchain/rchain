/*
  $ gcc -c lmdb_trans.c
  $ gcc -lm -pthread lmdb_trans.o liblmdb.a -o lmdb_trans.out
  $ mkdir testdb
  $ ./lmdb_trans.out
*/

int g_display = 0;

#define g_largestKey 50000

#define g_keyValuesCount 4

unsigned long g_mapSize = 2 * g_largestKey * (g_keyValuesCount + 1) * 16;
// sizeof(MDB_val) returns 16

char g_dbName[] = "lmdbtest";

char g_fileName[] = "lmdb_values_trans.txt";


#include "lmdb_code.h"


int NextKey(int usedKeys[])
{
  // check that usedKeys[] elements are not all non-zero
  int r = rand() % g_largestKey;
  while (usedKeys[r] != 0)
    r = rand() % g_largestKey;
  usedKeys[r] = 1;
  return r;
}

void ResetUsedKeys(int usedKeys[])
{
  int i = 0; for (i = 0; i < g_largestKey; ++i) usedKeys[i] = 0;
}

// must compile with -lm flag
long MillisSinceEpoch()
{
  struct timeval t;
  gettimeofday(&t, NULL);
  return round((t.tv_sec + (t.tv_usec / 1000000.0)) * 1000.0);
}

int *Init(char *testDir)
{
  char currDir[5012] = "";
  getcwd(currDir, 5011);
  char testPath[5012] = "";
  // remove data.mdb and lock.mdb
  sprintf(testPath, "%s/%s/%s", currDir, testDir, "data.mdb");
  remove(testPath);
  sprintf(testPath, "%s/%s/%s", currDir, testDir, "lock.mdb");
  remove(testPath);

  int *usedKeys = (int *) malloc(g_largestKey * sizeof(int));
  ResetUsedKeys(usedKeys);

  srand(time(NULL)); // should only be called once

  return usedKeys;
}

void Assert(int rc, int line)
{
  if (rc != MDB_SUCCESS)
    printf("line: %d, rc is not MDB_SUCCESS, it is: %d\n", line, rc);
  assert(rc == MDB_SUCCESS);
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


void main(int argc,char * argv[])
{
  char lines[3][64];
  sprintf(lines[0], "number of keys: %ld", g_largestKey);
  sprintf(lines[1], "values/key: %ld", g_keyValuesCount);
  sprintf(lines[2], "map size: %ld", g_mapSize);
  Output(lines, 3);

  char testDir[] = "testdb";

  // code modified from mtest*.c
  MDB_env *env;
  MDB_dbi dbi;
  MDB_val key, value;
  MDB_txn *txn;
  MDB_stat mst;
  MDB_cursor *cursor;
  int kBuf = -1;
  int vBuf = -1;
  
  key.mv_size = sizeof(kBuf);
  key.mv_data = &kBuf;
  value.mv_size = sizeof(vBuf);
  value.mv_data = &vBuf;

  int *usedKeys = Init(testDir);

  int rc = mdb_env_create(&env); Assert(rc, __LINE__);
  rc = mdb_env_set_mapsize(env, g_mapSize); Assert(rc, __LINE__);
  rc = mdb_env_set_maxdbs(env, 1); /* necessary */ Assert(rc, __LINE__);
  rc = mdb_env_open(env, "./testdb", MDB_WRITEMAP, 0664); Assert(rc, __LINE__);

  int *values = (int *) malloc(g_keyValuesCount * sizeof(int));

  long startPut = MillisSinceEpoch();

  int keyCount = 0;
  for (keyCount = 0; keyCount < g_largestKey; ++keyCount)
  {
    int iKey = NextKey(usedKeys);
    int i = 0;
    for (i = 0; i < g_keyValuesCount; ++i) values[i] = iKey + 1 + i;   

    kBuf = iKey;
    if (g_display) printf("put key: %d, values: ", *((int *) key.mv_data));

    int iVal = 0;
    for (iVal = 0; iVal < g_keyValuesCount; ++iVal)
    {
      rc = mdb_txn_begin(env, NULL, 0, &txn); Assert(rc, __LINE__);

      int flags = MDB_DUPSORT;
      if (keyCount == 0 && iVal == 0)
        flags |= MDB_CREATE;
      rc = mdb_dbi_open(txn, g_dbName, flags, &dbi); Assert(rc, __LINE__);
      rc = mdb_cursor_open(txn, dbi, &cursor); Assert(rc, __LINE__);

      kBuf = iKey;
      // necessary because mbd_cursor_get() changes key.mv_data
      key.mv_data = &kBuf;

      vBuf = iKey + 1 + iVal;
      value.mv_data = &vBuf;

      if (g_display) printf("%d, ", *((int *) value.mv_data));

      rc = mdb_cursor_put(cursor, &key, &value, 0); Assert(rc, __LINE__);

      mdb_cursor_close(cursor);
      rc = mdb_txn_commit(txn); Assert(rc, __LINE__);
    }
    if (g_display) printf("\n");
  }
  if (g_display) printf("\n");

  long stopPut = MillisSinceEpoch();

  ResetUsedKeys(usedKeys);

  long startGet = MillisSinceEpoch();

  keyCount = 0;
  for (keyCount = 0; keyCount < g_largestKey; ++keyCount)
  {
    rc = mdb_txn_begin(env, NULL, MDB_RDONLY, &txn); Assert(rc, __LINE__);
    rc = mdb_dbi_open(txn, g_dbName, MDB_DUPSORT, &dbi); Assert(rc, __LINE__);
    rc = mdb_cursor_open(txn, dbi, &cursor); Assert(rc, __LINE__);

    int iKey = NextKey(usedKeys);

    // necessary because mbd_cursor_get() changes key.mv_data
    key.mv_data = &kBuf;
    kBuf = iKey;

    rc = mdb_cursor_get(cursor, &key, &value, MDB_SET_KEY);
    if (rc != MDB_SUCCESS)
    {
      if (g_display)
      {
        if (rc == -30798)
          printf("get key MDB_SET_KEY '%ld' not found, error: MDB_NOTFOUND\n", iKey);
        else
          printf("get key MDB_SET_KEY '%ld' not found, error: %d\n", iKey, rc);
      }
      mdb_cursor_close(cursor);
      mdb_txn_abort(txn);
      continue;
    }

    rc = mdb_cursor_get(cursor, &key, &value, MDB_FIRST_DUP);
    if (rc == MDB_NOTFOUND)
    {
      if (g_display)
      {
        if (rc == -30798)
          printf("get key MDB_FIRST_DUP '%ld' not found, error: MDB_NOTFOUND\n", iKey);
        else
          printf("get key MDB_FIRST_DUP '%ld' not found, error: %d\n", iKey, rc);
      }
      mdb_cursor_close(cursor);
      mdb_txn_abort(txn);
      continue;
    }

    if (g_display) printf("get key: %d, values: %d", *((int *) key.mv_data), *((int *) value.mv_data));
    rc = mdb_cursor_get(cursor, &key, &value, MDB_NEXT_DUP);

    while (rc != MDB_NOTFOUND)
    {
      if (g_display) printf(", %d", *((int *) value.mv_data));
      rc = mdb_cursor_get(cursor, &key, &value, MDB_NEXT_DUP);
    }
    if (g_display) printf("\n");

    mdb_cursor_close(cursor);
    mdb_txn_abort(txn);
  }
  if (g_display) printf("\n");

  // mdb_dbi_close(env, dbi);
  // mdb_env_close(env); // why? double free or corruption (!prev): 0x0000000001a80270 ***

  long stopGet = MillisSinceEpoch();

  Outcome(startPut, stopPut, startGet, stopGet);
}


