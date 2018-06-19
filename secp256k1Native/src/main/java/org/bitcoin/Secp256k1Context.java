/*
 * Copyright 2014-2016 the libsecp256k1 contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.bitcoin;

import org.apache.commons.lang3.SystemUtils;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;

/**
 * This class holds the context reference used in native methods 
 * to handle ECDSA operations.
 */
public class Secp256k1Context {
  private static final boolean enabled; //true if the library is loaded
  private static final long context; //ref to pointer to context obj


  static { //static initializer
      boolean isEnabled = true;
      long contextRef = -1;
      try {
          String fileName = getLibFile();
          URI uri = ClassLoader.getSystemClassLoader().getResource(fileName).toURI();
          String path = Paths.get(uri).toFile().getAbsolutePath();
          System.load(path);
          contextRef = secp256k1_init_context();
      } catch (UnsatisfiedLinkError e) {
          System.out.println("UnsatisfiedLinkError: " + e.toString());
          isEnabled = false;
      } catch (URISyntaxException e){
          System.out.println("Invalid URI format: " + e.toString());
          isEnabled = false;
      } catch (NullPointerException e) {
          System.out.println("Null pointer exception: " + e.toString());
          isEnabled = false;
      }
      enabled = isEnabled;
      context = contextRef;
  }

  private static String getLibFile() throws RuntimeException {
      if(SystemUtils.IS_OS_LINUX) {
          return "libsecp256k1.so";
      } else if(SystemUtils.IS_OS_MAC_OSX) {
          return "libsecp256k1.dylib";
      } else if(SystemUtils.IS_OS_WINDOWS) {
          return "secp256k1.dll";
      } else {
          throw new RuntimeException("Unrecognized operating system.");
      }
  }

  public static boolean isEnabled() {
     return enabled;
  }

  public static long getContext() {
     if(!enabled) return -1; //sanity check
     return context;
  }

  private static native long secp256k1_init_context();
}
