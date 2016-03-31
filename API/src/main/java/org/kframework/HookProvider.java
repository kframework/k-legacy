// Copyright (c) 2016 K Team. All Rights Reserved.
package org.kframework;

import com.google.inject.Provider;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;

import java.io.IOException;
import java.io.InputStream;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * HookProvider
 *
 * Copied from org.kframework.backend.java.symbolic.JavaSymbolicCommonModule
 */
public class HookProvider {

    private static final String HOOK_PROPERTIES_FILE_NAME = "/org/kframework/backend/java/symbolic/hooks.properties";

    private static Map<String, String> getHookDeclarations() {
        Properties properties = new Properties();
        try {
            InputStream inStream = KRunAPI.class.getResourceAsStream(HOOK_PROPERTIES_FILE_NAME);
            if (inStream == null) {
                throw new IOException("Could not find resource " + HOOK_PROPERTIES_FILE_NAME);
            }
            properties.load(inStream);
        } catch (IOException e) {
            throw KEMException.internalError("Could not read from resource " + HOOK_PROPERTIES_FILE_NAME, e);
        }
        Map<String, String> builtinMethods = new HashMap<>();
        for (Object o: properties.keySet()) {
            String key = (String) o;
            builtinMethods.put(key, properties.getProperty(key));
        }
        return builtinMethods;
    }

    private static Map<String, Provider<MethodHandle>> getBuiltinTable(Map<String, String> hookDeclarations, KExceptionManager kem) {
        Map<String, Provider<MethodHandle>> result = new HashMap<>();
        MethodHandles.Lookup lookup = MethodHandles.lookup();
        for (String key : hookDeclarations.keySet()) {
            String hook = hookDeclarations.get(key);
            try {
                String className = hook.substring(0, hook.lastIndexOf('.'));
                String methodName = hook.substring(hook.lastIndexOf('.') + 1);
                Class<?> c = Class.forName(className);
                for (Method method : c.getDeclaredMethods()) {
                    if (method.getName().equals(methodName)) {
                        MethodHandle handle = lookup.unreflect(method);
                        result.put(key, () -> {
                            MethodHandle resultHandle = handle;
                            /* TODO: support non-static hooks e.g., z3 bindings
                            if (!Modifier.isStatic(method.getModifiers())) {
                                try {
                                    resultHandle = MethodHandles.insertArguments(handle, 0 , injector.getInstance(c));
                                } catch (KEMException e) {
                                    e.exception.addTraceFrame("while constructing implementation for " + hook + " hook.");
                                    throw e;
                                }
                            }
                             */
                            return resultHandle;
                        });
                        break;
                    }
                }
            } catch (ClassNotFoundException | SecurityException | IllegalAccessException e) {
                kem.registerCriticalWarning("missing implementation for hook " + key + ":\n" + hook, e);
            }
        }
        return result;
    }

    public static Map<String, Provider<MethodHandle>> get(KExceptionManager kem) {
        Map<String, String> hookDeclarations = getHookDeclarations();
        return getBuiltinTable(hookDeclarations, kem);
    }

}
