// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.parser.generator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.kframework.backend.Backend;
import org.kframework.kil.ASTNode;
import org.kframework.kil.DefinitionItem;
import org.kframework.kil.Module;
import org.kframework.kil.Require;
import org.kframework.kil.Sources;
import org.kframework.kil.loader.Context;
import org.kframework.main.GlobalOptions;
import org.kframework.parser.outer.Outer;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import com.google.inject.Inject;
import org.kframework.utils.file.JarInfo;

public class OuterParser {
    private List<DefinitionItem> moduleItems;
    private Map<String, Module> modulesMap;
    private List<String> filePaths;
    private File mainFile;
    private String mainModule;
    private final boolean autoinclude;
    private static final String missingFileMsg = "Could not find 'required' file: ";

    private final GlobalOptions globalOptions;
    private final String autoincludedFile;
    private final FileUtil files;
    private final KExceptionManager kem;

    @Inject
    public OuterParser(
            GlobalOptions globalOptions,
            @Backend.Autoinclude boolean autoinclude,
            @Backend.AutoincludedFile String autoincludedFile,
            FileUtil files,
            KExceptionManager kem) {
        this.autoinclude = autoinclude;
        this.globalOptions = globalOptions;
        this.autoincludedFile = autoincludedFile;
        this.files = files;
        this.kem = kem;
    }

    /**
     * Given a file, this method parses it and creates a list of modules from all of the included files.
     */
    public void slurp(String fileName, Context context) {
        moduleItems = new ArrayList<DefinitionItem>();
        modulesMap = new HashMap<String, Module>();
        filePaths = new ArrayList<String>();

        try {
            // parse first the file given at console for fast failure in case of error
            File file = files.resolveWorkingDirectory(fileName);
            if (!file.exists())
                throw KExceptionManager.criticalError(missingFileMsg + fileName + " given at console.");

            slurp2(file, context, false);

            if (autoinclude) {
                // parse the autoinclude.k file but remember what I parsed to give the correct order at the end
                List<DefinitionItem> tempmi = moduleItems;
                moduleItems = new ArrayList<DefinitionItem>();

                File autoinclude = buildCanonicalPath(autoincludedFile, file);
                if (!autoinclude.exists())
                    throw KExceptionManager.criticalError(missingFileMsg + autoinclude + " autoimported for every definition ");

                slurp2(autoinclude, context, true);
                moduleItems.addAll(tempmi);
            }

            setMainFile(file);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Calculates the path of a required file depending on the current file location.
     * First of all look relatively to the current file location. If the file doesn't exist
     * look in the /include directory. This allows for an easy way to avoid including
     * the predefined files. Just add an autoinclude.k in the current working directory.
     * @param fileName   name of the required file, as a string.
     * @param parentFile File object of the current file.
     * @return File object of the found file, or null if the file couldn't be found.
     * @throws IOException
     */
    private File buildCanonicalPath(String fileName, File parentFile) throws IOException {
        File file = new File(parentFile.getCanonicalFile().getParent() + "/" + fileName);
        if (file.exists())
            return file;
        file = new File(JarInfo.getKBase(false) + "/include/" + fileName);
        if (file.exists())
            return file;

        return null;
    }

    private void slurp2(File file, Context context, boolean predefined) throws IOException {
        String canonicalPath = file.getCanonicalPath();
        if (!filePaths.contains(canonicalPath)) {
            filePaths.add(canonicalPath);

            if (globalOptions.verbose)
                System.out.println("Including file: " + file.getAbsolutePath());
            List<DefinitionItem> defItemList = Outer.parse(Sources.fromFile(file), FileUtils.readFileToString(file), context);

            // go through every required file
            for (ASTNode di : defItemList) {
                if (di instanceof Require) {
                    Require req = (Require) di;

                    File newFile = buildCanonicalPath(req.getValue(), file);
                    boolean predefinedRequirement = predefined;
                    if (!newFile.exists()) {
                        predefinedRequirement = true;
                        newFile = files.resolveKBase("include/" + req.getValue());
                    }

                    if (!newFile.exists())
                        throw KExceptionManager.criticalError(missingFileMsg + req.getValue(), req);

                    slurp2(newFile, context, predefinedRequirement);
                }
            }

            // add the modules to the modules list and to the map for easy access
            for (DefinitionItem di : defItemList) {
                if (predefined)
                    di.setPredefined(true);

                this.moduleItems.add(di);
                if (di instanceof Module) {
                    Module m = (Module) di;
                    Module previous = this.modulesMap.put(m.getName(), m);
                    if (previous != null) {
                        String msg = "Found two modules with the same name: " + m.getName();
                        throw KExceptionManager.criticalError(msg, m);
                    }
                }
            }
        }
    }

    public void setMainFile(File mainFile) {
        this.mainFile = mainFile;
    }

    public File getMainFile() {
        return mainFile;
    }

    public void setMainModule(String mainModule) {
        this.mainModule = mainModule;
    }

    public String getMainModule() {
        return mainModule;
    }

    public List<DefinitionItem> getModuleItems() {
        return moduleItems;
    }

    public void setModuleItems(List<DefinitionItem> moduleItems) {
        this.moduleItems = moduleItems;
    }

    public Map<String, Module> getModulesMap() {
        return modulesMap;
    }

    public void setModulesMap(Map<String, Module> modulesMap) {
        this.modulesMap = modulesMap;
    }
}
