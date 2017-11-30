/*
  $ gcc -c lmdb_test.c
  $ gcc -lm -pthread lmdb_test.o liblmdb.a -o lmdb_test.out
  $ mkdir testdb
  $ ./lmdb_test.out
*/

const int g_display = 1;


#define g_largestKey 1000
#define g_blobLength 104857

const unsigned long g_mapSize = (g_largestKey + g_largestKey/2) * (16 + g_blobLength);

// A blob is a sequence of characters (no trailing '\0').
// A blob is not a string.
const char *g_blobs[10];

const char g_fileName[] = "lmdb_blob.txt";

const char g_dbDirName[] = "lmdbtest";
const char g_dbName[] = "lmdbtest1";

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


void main(int argc, char * argv[])
{
  char lines[4][64];
  sprintf(lines[0], "number of keys: %ld", g_largestKey);
  sprintf(lines[1], "values/key: 1");
  sprintf(lines[2], "blob length: %ld", g_blobLength);
  sprintf(lines[3], "map size: %ld", g_mapSize);
  Output(lines, 4);

  char testDir[] = "testdb";

  // code modified from mtest*.c
  MDB_env *env;
  MDB_dbi dbi;
  MDB_val key, value;
  MDB_txn *txn;
  MDB_stat mst;
  MDB_cursor *cursor;
  unsigned long kBuf = -1;
  char vBuf[g_blobLength];
  
  key.mv_size = sizeof(kBuf);
  key.mv_data = &kBuf;
  value.mv_size = sizeof(g_blobLength);
  // value.mv_data = NULL;
  value.mv_data = vBuf;

  unsigned long *usedKeys = Init(testDir);

  int rc = mdb_env_create(&env); Assert(rc, __LINE__);
  rc = mdb_env_set_mapsize(env, g_mapSize); Assert(rc, __LINE__);
  rc = mdb_env_set_maxdbs(env, 1); /* necessary */ Assert(rc, __LINE__);
  rc = mdb_env_open(env, "./testdb", MDB_WRITEMAP, 0664); Assert(rc, __LINE__);

  rc = mdb_txn_begin(env, NULL, 0, &txn); Assert(rc, __LINE__);
  rc = mdb_dbi_open(txn, g_dbDirName, MDB_CREATE, &dbi); Assert(rc, __LINE__);
  rc = mdb_cursor_open(txn, dbi, &cursor); Assert(rc, __LINE__);

  long startPut = MillisSinceEpoch();

  unsigned long keyCount = 0;
  for (keyCount = 0; keyCount < g_largestKey; ++keyCount)
  {
printf("keyCount: %ld\n", keyCount);
    unsigned long iKey = NextKey(usedKeys);
printf("iKey: %ld\n", iKey);
    kBuf = iKey;
    // if (g_display) printf("key: %ld, values: ", *((unsigned long *) key.mv_data));
    if (g_display) printf("key: %ld, values: ", kBuf);

    // necessary because mbd_cursor_get() changes key.mv_data
    key.mv_data = &kBuf;
    kBuf = iKey;

    value.mv_data = (void *) g_blobs[iKey];
    if (g_display) printf("%c\n", g_blobs[iKey][0]);

    rc = mdb_cursor_put(cursor, &key, &value, 0); Assert(rc, __LINE__);
  }
  if (g_display) printf("x\n");

  long stopPut = MillisSinceEpoch();

  ResetUsedKeys(usedKeys);

  long startGet = MillisSinceEpoch();

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

  Outcome(startPut, stopPut, startGet, stopGet);

  mdb_cursor_close(cursor);
  mdb_txn_abort(txn);
  // mdb_dbi_close(env, dbi);
  mdb_env_close(env);
}

