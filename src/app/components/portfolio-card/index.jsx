'use client';

import PDFViewer from "../pdf-viewer";

const PortfolioCard = ({title, description, pdf, rFile}) => {
  const maxWidth = 280;
  const buttonClasses = "bg-cyan-600 p-2 text-white transition";

  const onDownloadPDF = () => {
    const link = document.createElement("a");
    link.href = pdf;
    link.download = pdf;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  };

  const onDownloadRCode = () => {
    const link = document.createElement("a");
    link.href = rFile;
    link.download = rFile;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  };

  return (
    <div className="text-center">
      <h3 className="text-2xl mb-2 font-medium line-clamp-2">{title}</h3>
      <p className="mb-2 line-clamp-3">{description}</p>
      <div className="group relative before:duration-300 before:transition before:z-10 before:w-full before:h-full before:absolute hover:before:bg-black hover:before:bg-opacity-50 before:left-0 before:top-0">
        <PDFViewer file={pdf} maxWidth={maxWidth} portfolio />
        <div className="z-20 absolute duration-500 top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 flex flex-col gap-y-4 opacity-0 group-hover:opacity-100 transition">
          { pdf && <button onClick={onDownloadPDF} className={buttonClasses}>Download PDF</button>}
          { rFile && <button onClick={onDownloadRCode} className={buttonClasses}>Download R Code</button>}
        </div>
      </div>
    </div>
  );
};

export default PortfolioCard;
