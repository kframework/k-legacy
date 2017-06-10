// Copyright (c) 2012-2016 K Team. All Rights Reserved.
package org.kframework.utils.file;

import com.google.common.io.Files;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.kframework.kompile.SerializableKoreDefinition;
import org.kframework.kore.Definition;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.nustaq.serialization.FSTConfiguration;
import org.nustaq.serialization.FSTObjectInput;
import org.nustaq.serialization.FSTObjectOutput;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.Reader;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;

public class FileUtil {

    private final File tempDir;
    private final File kompiledDir;
    private final File workingDir;
    private final File definitionDir;
    private final GlobalOptions options;
    private final Map<String, String> env;

    public static final String KORE_TXT = "kore.txt";
    public static final String KORE_BIN = "kore.bin";
    public static final String KOMPILED_DEFINITION_BIN = "extras/kompiledDefinition.bin";
    public static final String KOMPILE_OPTIONS_BIN = "extras/kompileOptions.bin";
    public static final String KOMPILE_META_INFO_TXT = "extras/kompileMetaInfo.txt";
    public static final String PARSER_GENERATOR_BIN = "extras/parserGenerator.bin";
    public static final String PARSED_DEFINITION_BIN = "extras/parsedDefinition.bin";
    public static final String EXTRAS_FOLDER = "extras/";
    public static final String TOP_CELL_INITIALIZER_BIN = "extras/topCellInitializer.bin";
    public static final String CACHE_BIN = "extras/cache.bin";
    public static final String TIMESTAMP = "extras/timestamp";

    final FSTConfiguration fstConfiguration;

    public FileUtil(
            File tempDir,
            File definitionDir,
            File workingDir,
            File kompiledDir,
            GlobalOptions options,
            Map<String, String> env) {
        this.tempDir = tempDir;
        this.definitionDir = definitionDir;
        this.workingDir = workingDir;
        this.kompiledDir = kompiledDir;
        this.options = options;
        this.env = env;
        fstConfiguration = FSTConfiguration.createDefaultConfiguration();
    }

    public static FileUtil get(GlobalOptions globalOptions, Map<String, String> env) {
        File tmp = Files.createTempDir();
        File tempDir = new File(tmp.getAbsolutePath() + File.pathSeparator + "tempDir");
        File definitionDir = new File(tmp.getAbsolutePath() + File.pathSeparator + "definitionDir");
        File workingDir = new File("."); // new File(tmp.getAbsolutePath() + File.pathSeparator + "workingDir");
        File kompiledDir = new File(tmp.getAbsolutePath() + File.pathSeparator + "kompiledDir");
        if (!tempDir.mkdir() || !definitionDir.mkdir() /* || !workingDir.mkdir() */ || !kompiledDir.mkdir()) {
            throw new AssertionError("Could not create one of the temporary directories");
        }
        return new FileUtil(tempDir, definitionDir, workingDir, kompiledDir, globalOptions, env);
    }

    public static FileUtil testFileUtil() {
        File workingDir = new File(".");
        return new FileUtil(workingDir, workingDir, workingDir, workingDir, new GlobalOptions(), System.getenv());
    }

    public static String moduleDerivedParserPath(String moduleName) {
        return EXTRAS_FOLDER + moduleName + "_Parser.bin";
    }

    public ProcessBuilder getProcessBuilder() {
        ProcessBuilder pb = new ProcessBuilder().directory(workingDir);
        pb.environment().clear();
        pb.environment().putAll(env);
        return pb;
    }

    public Map<String, String> getEnv() {
        return env;
    }

    public void deleteTempDir(KExceptionManager kem) {
        if (!options.debug) {
            try {
                FileUtils.deleteDirectory(tempDir);
            } catch (IOException e) {
                kem.registerCriticalWarning("Failed to delete temporary directory", e);
            }
        }
    }

    /**
     * Get language name in uppercase (main module name) given the filename of definition.
     */
    public static String getMainModule(String filename) {
        return FilenameUtils.getBaseName(filename).toUpperCase();
    }

    // generate an unique name for a folder with the name dirName
    public static String generateUniqueFolderName(String dirName) {
        DateFormat df = new SimpleDateFormat("-yyyy-MM-dd-HH-mm-ss-");
        Date today = Calendar.getInstance().getTime();
        String reportDate = df.format(today);
        return dirName + reportDate + UUID.randomUUID().toString();
    }

    /**
     * Loads the properties from the given file into the given Properties object.
     */
    public static void loadProperties(Properties properties, Class<?> cls, String resourcePath) throws IOException {
        try (InputStream inStream = cls.getResourceAsStream(resourcePath)) {
            if (inStream == null) {
                throw new IOException("Could not find resource " + resourcePath);
            }
            properties.load(inStream);
        }
    }

    public void saveToDefinitionDirectory(String file, String content) {
        save(resolveDefinitionDirectory(file), content);
    }

    public String loadFromWorkingDirectory(String file) {
        return load(resolveWorkingDirectory(file));
    }

    public void saveToWorkingDirectory(String file, String content) {
        save(resolveWorkingDirectory(file), content);
    }

    public void saveToWorkingDirectory(String file, byte[] content) {
        save(resolveWorkingDirectory(file), content);
    }

    public String loadFromKompiled(String file) {
        return load(resolveKompiled(file));
    }

    public void saveToKompiled(String file, String content) {
        save(resolveKompiled(file), content);
    }

    public String loadFromTemp(String file) {
        return load(resolveTemp(file));
    }

    public byte[] loadBytesFromTemp(String file) {
        return loadBytes(resolveTemp(file));
    }

    public void saveToTemp(String file, String content) {
        save(resolveTemp(file), content);
    }

    public void saveToTemp(String file, byte[] content) {
        save(resolveTemp(file), content);
    }

    public String loadFromKBase(String file) {
        return load(resolveKBase(file));
    }

    public File resolveTemp(String file) {
        return new File(tempDir, file);
    }

    public File resolveKompiled(String file) {
        return new File(kompiledDir, file);
    }

    public File resolveDefinitionDirectory(String file) {
        return new File(definitionDir, file);
    }

    public File resolveWorkingDirectory(String file) {
        return resolveWorkingDirectory(new File(file));
    }

    public File resolveWorkingDirectory(File file) {
        return resolveWorkingDirectory(file, workingDir);
    }

    public static File resolveWorkingDirectory(File file, File workingDir) {
        if (file.isAbsolute()) return file;
        return new File(workingDir, file.getPath());
    }

    public File resolveKBase(String file) {
        return new File(JarInfo.getKBase(), file);
    }

    public void copyTempFileToDefinitionDirectory(String fromPath) {
        copyFileToDirectory(resolveTemp(fromPath), resolveDefinitionDirectory("."));
    }

    public void copyTempFileToKompiledDirectory(String fromPath) {
        copyFileToDirectory(resolveTemp(fromPath), resolveKompiled("."));
    }

    public void copyTempFileToKompiledFile(String fromPath, String toPath) {
        copyFile(resolveTemp(fromPath), resolveKompiled(toPath));
    }

    private void copyFile(File from, File to) {
        try {
            FileUtils.copyFile(from, to);
        } catch (IOException e) {
            throw KEMException.criticalError("Could not copy " + from + " to " + to, e);
        }
    }

    private void copyFileToDirectory(File from, File toDir) {
        try {
            FileUtils.copyFileToDirectory(from, toDir);
        } catch (IOException e) {
            throw KEMException.criticalError("Could not copy " + from + " to directory " + toDir, e);
        }
    }

    public static void save(File file, String content) {
        try {
            File dir = file.getAbsoluteFile().getParentFile();
            if (!dir.exists() && !dir.mkdirs()) {
                throw KEMException.criticalError("Could not create directory " + dir);
            }
            FileUtils.writeStringToFile(file, content);
        } catch (IOException e) {
            throw KEMException.criticalError("Could not write to file " + file.getAbsolutePath(), e);
        }
    }

    public static void save(File file, byte[] content) {
        try {
            File dir = file.getAbsoluteFile().getParentFile();
            if (!dir.exists() && !dir.mkdirs()) {
                throw KEMException.criticalError("Could not create directory " + dir);
            }
            FileUtils.writeByteArrayToFile(file, content);
        } catch (IOException e) {
            throw KEMException.criticalError("Could not write to file " + file.getAbsolutePath(), e);
        }
    }

    public static String load(File file) {
        try {
            return FileUtils.readFileToString(file);
        } catch (IOException e) {
            throw KEMException.criticalError("Could not read from file " + file.getAbsolutePath(), e);
        }
    }

    public static String load(File file, File workingDir) {
        return load(resolveWorkingDirectory(file, workingDir));
    }

    public static byte[] loadBytes(File file) {
        try {
            return FileUtils.readFileToByteArray(file);
        } catch (IOException e) {
            throw KEMException.criticalError("Could not read from file " + file.getAbsolutePath(), e);
        }
    }

    public static Pair<PipedInputStream, PipedOutputStream> pipeOutputToInput() {
        try {
            PipedOutputStream out = new PipedOutputStream();
            PipedInputStream in = new PipedInputStream(out);
            return Pair.of(in, out);
        } catch (IOException e) {
            throw KEMException.internalError("Error creating input/output pipe", e);
        }
    }

    public static Pair<PipedOutputStream, PipedInputStream> pipeInputToOutput() {
        try {
            PipedInputStream in = new PipedInputStream();
            PipedOutputStream out = new PipedOutputStream(in);
            return Pair.of(out, in);
        } catch (IOException e) {
            throw KEMException.internalError("Error creating input/output pipe", e);
        }
    }

    public static String read(Reader reader) {
        try {
            return IOUtils.toString(reader);
        } catch (IOException e) {
            throw KEMException.internalError("Error reading from " + reader, e);
        }
    }

    public Reader readFromWorkingDirectory(String path) {
        File f = resolveWorkingDirectory(path);
        try {
            return new FileReader(f);
        } catch (FileNotFoundException e) {
            throw KEMException.criticalError("Could not read from file " + f.getAbsolutePath(), e);
        }
    }


    public <T> void saveToKompiledFST(String path, Class<T> cls, Object content) {
        File f= resolveKompiled(path);
        try{
            FileOutputStream outputStream = new FileOutputStream(f);
            FSTObjectOutput fstObjectOutput = fstConfiguration.getObjectOutput(outputStream);
            fstObjectOutput.writeObject(cls.cast(content), cls);
            fstObjectOutput.flush();
            outputStream.close();
        } catch (Exception e) {
            throw KEMException.criticalError("Could not save to file " + f.getAbsolutePath(), e);
        }
    }

    // DISABLE EXCEPTION CHECKSTYLE
    public <T> T readFromKompiledFST(String path, Class<T> cls) {
        File f = resolveKompiled(path);
        try{
            FileInputStream fio = new FileInputStream(f);
            FSTObjectInput fstObjectInput= fstConfiguration.getObjectInput(fio);
            Object readObject = fstObjectInput.readObject(cls);
            fio.close();
            return cls.cast(readObject);
        } catch (Exception e) {
            throw KEMException.criticalError("Could not read from file " + f.getAbsolutePath(), e);
        }
    }
    // ENABLE EXCEPTION CHECKSTYLE
}
