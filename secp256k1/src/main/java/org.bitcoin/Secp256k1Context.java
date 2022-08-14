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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import static java.io.File.createTempFile;
import static java.lang.System.getProperty;
import static java.lang.Thread.currentThread;
import static java.util.Locale.ENGLISH;
import static java.util.Objects.requireNonNull;

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

        final String libToLoad;

        final String arch = getProperty("os.arch");
        final boolean arch64 = "x64".equals(arch) || "amd64".equals(arch)
                || "x86_64".equals(arch);

        final String os = getProperty("os.name");
        final boolean linux = os.toLowerCase(ENGLISH).startsWith("linux");
        final boolean osx = os.startsWith("Mac OS X");
        final boolean osx_arm = "aarch64".equals(arch);
        final boolean windows = os.startsWith("Windows");

        try {
            if (arch64 && linux) {
                libToLoad = extract("secp256k1-native-linux-x86_64.so");
            } else if (arch64 && osx) {
                libToLoad = extract("secp256k1-native-osx-x86_64.dylib");
            }  else if (arch64 && windows) {
                libToLoad = extract("secp256k1-native-windows-x86_64.dll");
            } else if (osx_arm) {
                libToLoad = extract("secp256k1-native-osx-aarch64.dylib");
            } else  {
                throw new RuntimeException("No secp256k1-native library to extract");
            }
            System.load(libToLoad);
            contextRef = secp256k1_init_context();
        } catch (UnsatisfiedLinkError e) {
            System.out.println("UnsatisfiedLinkError: " + e.toString());
            isEnabled = false;
        } catch (IOException e){
            System.out.println("IOException: " + e.toString());
            isEnabled = false;
        } catch (NullPointerException e) {
            System.out.println("Null pointer exception: " + e.toString());
            isEnabled = false;
        }
        enabled = isEnabled;
        context = contextRef;
    }

    @SuppressWarnings("PMD.AssignmentInOperand")
    private static String extract(final String name) throws IOException {
        final String suffix = name.substring(name.lastIndexOf('.'));
        final File file;
        try {
            file = createTempFile("secp256k1-native-library-", suffix);
            file.deleteOnExit();
            final ClassLoader cl = currentThread().getContextClassLoader();
            requireNonNull(cl.getResource(name), "Classpath resource not found");
            try (InputStream in = cl.getResourceAsStream(name);
                 OutputStream out = Files.newOutputStream(file.toPath())) {
                int bytes;
                final byte[] buffer = new byte[4_096];
                while (-1 != (bytes = in.read(buffer))) {
                    out.write(buffer, 0, bytes);
                }
            }
            return file.getAbsolutePath();
        } catch (final IOException e) {
            throw new IOException("Failed to extract " + name, e);
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