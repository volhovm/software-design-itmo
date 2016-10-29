package ru.akirakozov.sd.refactoring.servlet;

import java.io.IOException;
import java.sql.SQLException;

/**
 * @author volhovm
 */
@FunctionalInterface
public interface DBHandler<T> {
    void apply(T t) throws SQLException, IOException;
}
