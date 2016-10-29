package ru.akirakozov.sd.refactoring.servlet;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;

import ru.akirakozov.sd.refactoring.servlet.*;

/**
 * @author akirakozov
 */
public class GetProductsServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
        throws IOException {
        ServletCommon.doGoodies                                                   (
            response                                                              ,
            (Statement stmt) ->                                                   {
                ResultSet rs = stmt.executeQuery("SELECT * FROM PRODUCT")         ;
                ServletCommon.dumpItems(response, rs, "All items that we have")   ;});
    }
}
