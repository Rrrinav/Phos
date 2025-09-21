#pragma once

#include <string>
#include <vector>
#include <utility>
#include <sstream>
#include <chrono>
#include <format>
#include "../b_ldr.hpp"

extern const std::string TARGET;

namespace report_gen
{

// A simple helper to escape HTML special characters
std::string html_escape(const std::string &data)
{
    std::string buffer;
    buffer.reserve(data.size());
    for (size_t pos = 0; pos != data.size(); ++pos)
    {
        switch (data[pos])
        {
            case '&':
                buffer.append("&amp;");
                break;
            case '\"':
                buffer.append("&quot;");
                break;
            case '\'':
                buffer.append("&apos;");
                break;
            case '<':
                buffer.append("&lt;");
                break;
            case '>':
                buffer.append("&gt;");
                break;
            default:
                buffer.append(&data[pos], 1);
                break;
        }
    }
    return buffer;
}

// Replaces all occurrences of a placeholder in a string with a value
void replace_all(std::string &str, const std::string &from, const std::string &to)
{
    if (from.empty())
        return;
    size_t start_pos = 0;
    while ((start_pos = str.find(from, start_pos)) != std::string::npos)
    {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length();
    }
}

// Helper function to generate a single detail page for a test file
void generate_detail_page(const std::string &file_path, const std::string &diff, const std::string &report_dir,
                          const std::string &details_template)
{
    std::string details_page_name = bld::fs::get_stem(file_path, false) + ".html";
    std::string details_page_path = report_dir + "/" + details_page_name;

    std::string source_code, expected_output, actual_output;
    bld::fs::read_file(file_path, source_code);
    bld::fs::read_file(bld::fs::get_stem(file_path, true) + ".expected", expected_output);
    bld::read_process_output({TARGET, file_path}, actual_output);

    std::string details_content = details_template;
    replace_all(details_content, "{{TEST_FILE_PATH}}", file_path);
    replace_all(details_content, "{{SOURCE_CODE}}", html_escape(source_code));
    replace_all(details_content, "{{EXPECTED_OUTPUT}}", html_escape(expected_output));
    replace_all(details_content, "{{ACTUAL_OUTPUT}}", html_escape(actual_output));
    replace_all(details_content, "{{DIFF}}", html_escape(diff));

    bld::fs::write_entire_file(details_page_path, details_content);
}

// Generates the main HTML report from test results
void generate_html_report(const std::vector<std::string> &passed_files, const std::vector<std::pair<std::string, std::string>> &failed_info,
                          const std::string &report_dir = "report", const std::string &index_template_path = "./rep-gen/index.html",
                          const std::string &details_template_path = "./rep-gen/details_template.html")
{
    bld::fs::create_dir_if_not_exists(report_dir);

    std::string index_template, details_template;
    if (!bld::fs::read_file(index_template_path, index_template) || !bld::fs::read_file(details_template_path, details_template))
    {
        bld::log(bld::Log_type::ERR, "Could not read HTML template files.");
        return;
    }

    std::stringstream test_results_html;

    // --- Process Passed Tests ---
    for (const auto &file_path : passed_files)
    {
        std::string details_page_name = bld::fs::get_stem(file_path, false) + ".html";
        test_results_html << "<li><a href=\"" << details_page_name << "\"><span class=\"status-passed\">✔</span> " << file_path
                          << "</a></li>\n";
        generate_detail_page(file_path, "", report_dir, details_template);
    }

    // --- Process Failed Tests ---
    for (const auto &[file_path, diff] : failed_info)
    {
        std::string details_page_name = bld::fs::get_stem(file_path, false) + ".html";
        test_results_html << "<li><a href=\"" << details_page_name << "\"><span class=\"status-failed\">✖</span> " << file_path
                          << "</a></li>\n";
        generate_detail_page(file_path, diff, report_dir, details_template);
    }

    // --- Generate Final Index Page ---
    int passed_count = passed_files.size();
    int failed_count = failed_info.size();
    int total_count = passed_count + failed_count;

    auto const time = std::chrono::current_zone()->to_local(std::chrono::system_clock::now());
    std::string timestamp = std::format("{:%Y-%m-%d %H:%M:%S}", time);
    std::string summary_class = (failed_count == 0) ? "summary-passed" : "summary-failed";
    std::string summary_message = (failed_count == 0) ? std::format("All {} tests passed!", passed_count)
                                                      : std::format("{} passed, {} failed", passed_count, failed_count);

    std::string final_index = index_template;
    replace_all(final_index, "{{TIMESTAMP}}", timestamp);
    replace_all(final_index, "{{SUMMARY_CLASS}}", summary_class);
    replace_all(final_index, "{{SUMMARY_MESSAGE}}", summary_message);
    replace_all(final_index, "{{TOTAL_COUNT}}", std::to_string(total_count));
    replace_all(final_index, "{{PASSED_COUNT}}", std::to_string(passed_count));
    replace_all(final_index, "{{FAILED_COUNT}}", std::to_string(failed_count));
    replace_all(final_index, "{{TEST_RESULTS_LIST}}", test_results_html.str());

    bld::fs::write_entire_file(report_dir + "/index.html", final_index);
    bld::log(bld::Log_type::INFO, "HTML report generated in '" + report_dir + "/' directory.");
}

}  // namespace report_gen
