package ru.akirakozov.sd.refactoring.servlet;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.ResultSet;
import java.sql.Statement;

/**
 * @author akirakozov
 */
public class QueryServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request,
                         HttpServletResponse response) throws IOException {

        String command = request.getParameter("command");

        if ("max".equals(command)) {
            ServletCommon.doGoodies(response, (Statement stmt) -> {
                ResultSet rs =
                    stmt.executeQuery("SELECT * FROM PRODUCT ORDER BY PRICE DESC LIMIT 1");
                ServletCommon.dumpItems(response, rs, "Items with max price");});
        } else if ("min".equals(command)) {
            ServletCommon.doGoodies(response, (Statement stmt) -> {
                ResultSet rs =
                    stmt.executeQuery("SELECT * FROM PRODUCT ORDER BY PRICE LIMIT 1");
                ServletCommon.dumpItems(response, rs, "Items with min price");});
        } else if ("sum".equals(command)) {
            ServletCommon.doGoodies(response, (Statement stmt) -> {
                ResultSet rs = stmt.executeQuery("SELECT SUM(price) FROM PRODUCT");
                response.getWriter().println("<html><body>");
                response.getWriter().println("Summary price: ");
                if (rs.next()) response.getWriter().println(rs.getInt(1));
                response.getWriter().println("</body></html>");});
        } else if ("count".equals(command)) {
            ServletCommon.doGoodies(response, (Statement stmt) -> {
                ResultSet rs = stmt.executeQuery("SELECT COUNT(*) FROM PRODUCT");
                response.getWriter().println("<html><body>");
                response.getWriter().println("Number of products: ");
                if (rs.next()) response.getWriter().println(rs.getInt(1));
                response.getWriter().println("</body></html>");});
        } else {
            response.getWriter().println("Unknown command: " + command);
            response.setContentType("text/html");
            response.setStatus(HttpServletResponse.SC_OK);
        }
    }

}
