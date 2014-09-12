# the root.cert file
root_cert_content = ::File.open(::File.join(node['epgsql-test-setup']['test-data-dir'], 'root.crt')).read
file ::File.join(node['postgresql']['config']['data_directory'], 'root.crt') do
  content root_cert_content
end
